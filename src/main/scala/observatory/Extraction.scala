package observatory

import java.nio.file.{Path, Paths}
import java.time.{LocalDate, MonthDay}
import java.util.concurrent.{ExecutorService, Executors}

import fs2.{Strategy, Stream, Task, io, text}

import scala.util.Try

/**
  * 1st milestone: data extraction
  */
object Extraction {

  implicit val ioExecutorService: ExecutorService = Executors.newCachedThreadPool()
  implicit val strategy: Strategy = Strategy.fromFixedDaemonPool(8, threadName = "fs2-strategy")

  case class Stn(value: Int) extends AnyVal
  case class Wban(value: Int) extends AnyVal
  case class TempF(value: Double) extends AnyVal
  case class Lat(value: Double) extends AnyVal
  case class Lon(value: Double) extends AnyVal
  case class StationKey(stn: Option[Stn], wban: Option[Wban])

  case class Temperature(station: StationKey, monthDay: MonthDay, temp: TempF)
  case class Station(key: StationKey, latitude: Lat, longitude: Lon)

  def mkStationKey(stn: Option[Stn], wban: Option[Wban]): Option[StationKey] =
    if (stn.isDefined || wban.isDefined) Some(StationKey(stn, wban))
    else None

  def parseTemperature(line: String): Option[Temperature] = Try {
    val row = line.split(',')
    val monthDay = MonthDay.of(row(2).toInt, row(3).toInt)
    val temp = TempF(row(4).toDouble)
    val stationKey = mkStationKey(
      stn = Option(row(0)).filter(_.nonEmpty).map(s => Stn(s.toInt)),
      wban = Option(row(1)).filter(_.nonEmpty).map(s => Wban(s.toInt))
    )
    stationKey.map(Temperature(_, monthDay, temp))
  }.toOption.flatten

  def parseStation(line: String): Option[Station] = Try {
    val row = line.split(',')
    val stationKey = mkStationKey(
      stn = Option(row(0)).filter(_.nonEmpty).map(s => Stn(s.toInt)),
      wban = Option(row(1)).filter(_.nonEmpty).map(s => Wban(s.toInt))
    )
    for {
      key <- stationKey
      lat <- Option(row(2)).filter(_.nonEmpty).map(s => Lat(s.toDouble))
      lon <- Option(row(3)).filter(_.nonEmpty).map(s => Lon(s.toDouble))
    } yield Station(key, lat, lon)
  }.toOption.flatten

  def stations(stationsPath: Path): Task[Map[StationKey, Station]] =
    io.file.readAll[Task](stationsPath, chunkSize = 1024 * 100)
      .through(text.utf8Decode)
      .through(text.lines)
      .map(parseStation)
      .collect { case Some(station) => station }
      .runFold(Map.empty[StationKey, Station]) { (acc, station) =>
        acc.updated(station.key, station)
      }

  def temperatures(temperaturesPath: Path): Stream[Task, Temperature] =
    io.file.readAllAsync[Task](temperaturesPath, chunkSize = 1024 * 1024, Some(ioExecutorService))
      .through(text.utf8Decode)
      .through(text.lines)
      .map(parseTemperature)
      .collect { case Some(row) => row }

  def fahrenheitToCelsius(fahrenheit: Double): Double = (fahrenheit - 32.0) * (5.0 / 9.0)

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    val stationMap = stations(Paths.get(getClass.getResource(stationsFile).toURI)).unsafeRun()
    temperatures(Paths.get(getClass.getResource(temperaturesFile).toURI))
      .flatMap {
        case Temperature(key, monthDay, TempF(fahrenheit)) =>
          stationMap.get(key).map { case Station(_, Lat(lat), Lon(lon)) =>
              val location = Location(lat, lon)
              val localDate = LocalDate.of(year, monthDay.getMonth, monthDay.getDayOfMonth)
              val tempC = fahrenheitToCelsius(fahrenheit)
              Stream((localDate, location, tempC))
            }.getOrElse(Stream.empty)
        case _ =>
          Stream.empty
      }
      .runLog
      // TODO: Better way to convert stream to iterable
      // https://github.com/arkig/spream/blob/master/src/main/scala/spream/stream/Conversions.scala
      .unsafeRun()
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    records.groupBy(_._2).map {
      case (location, group) =>
        val (cnt, sum) = group.map(_._3).foldLeft(0 -> 0.0){ case ((c, s), t) => (c + 1, s + t) }
        (location, sum / cnt)
    }
  }

}
