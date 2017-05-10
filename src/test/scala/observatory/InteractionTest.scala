package observatory

import java.nio.file.Paths

import observatory.Extraction._
import observatory.Interaction._
import observatory.Visualization.Loc
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

import scala.collection.mutable
import scala.collection.parallel.ParIterable

@RunWith(classOf[JUnitRunner])
class InteractionTest extends FunSuite with Checkers {

  val colors = Array(
    -60.0 -> Color(0,   0,   0  ),
    -50.0 -> Color(33,  0,   107),
    -27.0 -> Color(255, 0,   255),
    -15.0 -> Color(0,   0,   255),
    0.0   -> Color(0,   255, 255),
    12.0  -> Color(255, 255, 0  ),
    32.0  -> Color(255, 0,   0  ),
    60.0  -> Color(255, 255, 255)
  )

  test("tile000") {
    val stationMap = stations(Paths.get(getClass.getResource("/stations_2021.csv").toURI)).unsafeRun()

    val temperatures = locationYearlyAverageRecordsInt(locateTemperaturesInt(year = 2021, stationMap, "/2021.csv"))
    tileInt(temperatures, colors, zoom = 0, x = 0, y = 0).output("/tmp/tile000.png")
    tileInt(temperatures, colors, zoom = 1, x = 0, y = 0).output("/tmp/tile100.png")
    tileInt(temperatures, colors, zoom = 1, x = 1, y = 0).output("/tmp/tile110.png")
    tileInt(temperatures, colors, zoom = 1, x = 0, y = 1).output("/tmp/tile101.png")
    tileInt(temperatures, colors, zoom = 1, x = 1, y = 1).output("/tmp/tile111.png")
    ()
  }

  test("tiles") {
    val tempMap = mutable.Map[Int, ParIterable[(Loc, Double)]]()
    val stationMap = stations(Paths.get(getClass.getResource("/stations.csv").toURI)).unsafeRun()

    def generateImage[D](year: Int, zoom: Int, x: Int, y: Int, data: D): Unit = {
      val temperatures = tempMap(year)
//      val dir = s"target/temperatures/128/$year/$zoom"
      val dir = s"target/temperatures/$year/$zoom"
      new java.io.File(dir).mkdirs
      tileInt(temperatures, colors, zoom, x, y).output(s"$dir/$x-$y.png")
      println(s"Generated tile for year $year  zoom $zoom  x = $x  y = $y")
      ()
    }

    def loadYearAvgTemps(year: Int) =
      tempMap.update(year, locationYearlyAverageRecordsInt(locateTemperaturesInt(year, stationMap, s"/$year.csv")))

    val years = 1975 to 2015
//    val years = Seq(1982)
    years.foreach(loadYearAvgTemps)
    generateTilesInt[Unit](years.map(y => (y, ())), generateImage, zRange = 0 to 3)
  }
}
