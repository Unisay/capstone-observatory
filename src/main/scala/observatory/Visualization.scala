package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import scala.collection.parallel.ParIterable
import scala.math._

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  val earthRadiusMeters = 6376774
  val powerParameter = 6.0

  def fastACos(a: Double): Double = {
    val negate = if (a < 0) 1.0 else 0.0
    val x = abs(a)
    val y = (((-0.0187293 * x + 0.0742610) * x - 0.2121144) * x + 1.5707288) * sqrt(1.0 - x)
    val z = y - 2 * negate * y
    negate * 3.14159265358979 + z
  }

  implicit class RichDouble(val d: Double) extends AnyVal {
    def bound(l: Double, h: Double): Double = max(l, min(h, d))
  }

  case class Loc(lat: Double, lon: Double) {
    val latR: Double = lat.toRadians
    val lonR: Double = lon.toRadians
    val sinLat: Double = math.sin(latR)
    val cosLat: Double = math.cos(latR)
    def toLocation = Location(lat, lon)
  }

  object Loc {
    def apply(location: Location): Loc = Loc(location.lat, location.lon)
  }

  def greatCircleDistance(a: Loc, b: Loc): Double = {
    val angle = a.sinLat * b.sinLat + a.cosLat * b.cosLat * cos(a.lonR - b.lonR)
    earthRadiusMeters * fastACos(angle.bound(-1, +1))
  }

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double =
    predictTemperatureInt(temperatures.map { case (l, t) => (Loc(l), t) }.par)(Loc(location))

  def predictTemperatureInt(temps: ParIterable[(Loc, Double)])(location: Loc): Double = {
    assert(temps.nonEmpty, "No temperatures available")
    val (numerator, denominator) = temps.foldLeft((0.0, 0.0)) {
      case ((num, den), (loc, temp)) =>
        val gcd = greatCircleDistance(location, loc)
        val wix = 1.0 / math.pow(gcd, powerParameter)
        val _num = num + wix * temp
        val _den = den + wix
        if (gcd < 1000)
          return temp // early abort fold
       else (_num, _den)
    }
    assert(denominator != 0.0, "Zero denominator")
    numerator / denominator
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
    val scale = points.toVector
    val len = scale.length
    if (len == 0) {
      sys.error("Empty color scale")
    } else if (len == 1) {
       scale.head._2
    } else {
      pixelToColor(interpolateColorInt(scale.sortBy(_._1).toArray)(value))
    }
  }

  def colorToPixel(color: Color): Pixel = Pixel(color.red, color.green, color.blue, alpha = 255)
  def pixelToColor(pixel: Pixel): Color = Color(pixel.red, pixel.green, pixel.blue)

  def interpolateColorInt(sorted: Array[(Double, Color)], alpha: Int = 255)(value: Double): Pixel = {
    val first = sorted.head
    val last = sorted.last
    if (first._1 > value) {
      colorToPixel(first._2)
    } else if (last._1 < value) {
      colorToPixel(last._2)
    } else {
      val before = sorted.takeWhile(_._1 < value)
      if (before.isEmpty) return colorToPixel(first._2)
      val after = sorted.dropWhile(_._1 < value)
      if (after.isEmpty) return colorToPixel(last._2)
      val a = before.last
      val b = after.head
      def interpolateColor(startColor: Int, stopColor: Int, x0: Double, x1: Double, x: Double) =
        math.round((startColor * (x1 - x) + stopColor * (x - x0)) / (x1 - x0)).toInt
      Pixel(
        interpolateColor(a._2.red,   b._2.red,   a._1, b._1, value),
        interpolateColor(a._2.green, b._2.green, a._1, b._1, value),
        interpolateColor(a._2.blue,  b._2.blue,  a._1, b._1, value),
        alpha
      )
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image =
    visualizeInt(temperatures.map { case (l, t) => (Loc(l), t) }, colors)

  def visualizeInt(temperatures: Iterable[(Loc, Double)], colors: Iterable[(Double, Color)]): Image = {
    val locations = for {
      lat <- 90 until -90 by -1
      lon <- -180 until 180
    } yield Loc(lat, lon)

    assert(locations.length == 64800, "Wrong number of pixels")

    val temps = temperatures.toParArray
    val colorScale = colors.toArray.sortBy(_._1)

    val pixels = locations
      .par
      .map(predictTemperatureInt(temps))
      .map(interpolateColorInt(colorScale))
      .toArray

    Image(w = 360, h = 180, pixels)
  }

}

