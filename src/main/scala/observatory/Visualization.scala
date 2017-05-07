package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import Math._

import scala.collection.parallel.mutable.ParArray

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  val earthRadiusMeters = 6372800
  val powerParameter = 2.0

  def greatCircleDistance(a: Location, b: Location): Double = {
    val dLat = (b.lat - a.lat).toRadians
    val dLon = (b.lon - a.lon).toRadians
    val d = pow(sin(dLat / 2.0), 2.0) + pow(sin(dLon / 2.0), 2.0) * cos(a.lat.toRadians) * cos(b.lat.toRadians)
    earthRadiusMeters * (2 * asin(sqrt(d)))
  }

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double =
    predictTemperaturePar(temperatures.toParArray)(location)

  def predictTemperaturePar(temps: ParArray[(Location, Double)])(location: Location): Double = {
    val (numerator, denominator) = temps.foldLeft((0.0, 0.0)) {
      case ((num, den), (loc, temp)) =>
        val gcd = greatCircleDistance(location, loc)
        val wix = 1.0 / pow(gcd, powerParameter)
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
      pixelToColor(interpolatePixel(scale.sortBy(_._1).toArray)(value))
    }
  }

  def colorToPixel(color: Color): Pixel = Pixel(color.red, color.green, color.blue, alpha = 255)
  def pixelToColor(pixel: Pixel): Color = Color(pixel.red, pixel.green, pixel.blue)

  def interpolatePixel(sorted: Array[(Double, Color)])(value: Double): Pixel = {
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
        round((startColor * (x1 - x) + stopColor * (x - x0)) / (x1 - x0)).toInt
      Pixel(
        interpolateColor(a._2.red,   b._2.red,   a._1, b._1, value),
        interpolateColor(a._2.green, b._2.green, a._1, b._1, value),
        interpolateColor(a._2.blue,  b._2.blue,  a._1, b._1, value),
        255
      )
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    val locations = for {
      lat <- 90 until -90 by -1
      lon <- -180 until 180
    } yield Location(lat, lon)

    assert(locations.length == 64800, "Wrong number of pixels")

    val temps = temperatures.toParArray
    val colorScale = colors.toArray.sortBy(_._1)

    val pixels = locations
      .par
      .map(predictTemperaturePar(temps))
      .map(interpolatePixel(colorScale))
      .toArray

    Image(w = 360, h = 180, pixels)
  }

}

