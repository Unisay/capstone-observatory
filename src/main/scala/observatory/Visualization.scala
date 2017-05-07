package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import Math._

import scala.collection.parallel.mutable.ParArray

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  val earthRadiusMeters = 6372800
  val powerParameter = 4.0

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
    val (closestTemperature, minDistanceMeters) = temps.foldLeft((Double.MaxValue, Double.MaxValue)) {
      case (same @ (_, minDistance), (nextLocation, nextTemp)) =>
        val nextDistance = greatCircleDistance(location, nextLocation)
        if (nextDistance < minDistance) (nextTemp, nextDistance) else same
    }
    if (minDistanceMeters <= 1000) {
      closestTemperature
    } else {
      def wi(x: Location, xi: Location): Double = 1.0 / pow(greatCircleDistance(x, xi), powerParameter)
      val (numerator, denominator) = temps.foldLeft((0.0, 0.0)) {
        case ((num, den), (loc, temp)) =>
          val wix = wi(location, loc)
          (num + wix * temp, den + wix)
      }
      assert(denominator != 0.0, "Zero denominator")
      numerator / denominator
    }
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
      interpolateColorUnsafe(scale.sortBy(_._1).toArray)(value)
    }
  }

  def interpolateColorUnsafe(sorted: Array[(Double, Color)])(value: Double): Color = {
    val first = sorted.head
    val last = sorted.last
    if (first._1 > value) {
      first._2
    } else if (last._1 < value) {
      last._2
    } else {
      val before = sorted.takeWhile(_._1 < value)
      if (before.isEmpty) return sorted.head._2
      val after = sorted.dropWhile(_._1 < value)
      if (after.isEmpty) return sorted.last._2
      val a = before.last
      val b = after.head
      def li(y0: Int, x0: Double, y1: Int, x1: Double, x: Double): Int =
        round((y0 * (x1 - x) + y1 * (x - x0)) / (x1 - x0)).toInt
      Color(
        red   = li(a._2.red,   a._1, b._2.red,   b._1, value),
        green = li(a._2.green, a._1, b._2.green, b._1, value),
        blue  = li(a._2.blue,  a._1, b._2.blue,  b._1, value)
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
      .toParArray
      .map(predictTemperaturePar(temps))
      .map(interpolateColorUnsafe(colorScale))
      .map(c => Pixel(c.red, c.green, c.blue, alpha = 255))
      .toArray

    Image(w = 360, h = 180, pixels)
  }

}

