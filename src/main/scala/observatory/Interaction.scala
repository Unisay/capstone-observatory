package observatory

import scala.math._
import com.sksamuel.scrimage.Image
import observatory.Visualization.{Loc, _interpolateColor, _predictTemperature}

import scala.collection.parallel.ParIterable

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(zoom: Int, x: Int, y: Int): Location =
    tileLoc(zoom, x, y).toLocation

  def tileLoc(zoom: Int, x: Int, y: Int): Loc =
    Loc(
      lat = toDegrees(atan(sinh(Pi * (1.0 - 2.0 * y.toDouble / (1 << zoom))))),
      lon = x.toDouble / (1 << zoom) * 360.0 - 180.0
    )

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return A 256×256 image showing the contents of the tile defined by `x`, `y` and `zooms`
    */
  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image = {
    val colorScale = colors.toArray.sortBy(_._1)
    tileInt(temperatures.par.map { case (l, t) => (Loc(l), t) }, colorScale, zoom, x, y)
  }

  def tileInt(
    temperatures: ParIterable[(Loc, Double)],
    colors: Array[(Double, Color)],
    zoom: Int,
    x: Int,
    y: Int,
    tileSize: Int = 256
  ): Image = {
    val z = zoom + 8
    val locations = for {
      ty <- 0 until tileSize
      tx <- 0 until tileSize
    } yield tileLoc(z, x * tileSize + tx, y * tileSize + ty)

    assert(locations.length == tileSize * tileSize, "Wrong number of pixels")

    val pixels = locations
      .par
      .map(_predictTemperature(temperatures))
      .map(_interpolateColor(colors, alpha = 127))
      .toArray

    Image(w = tileSize, h = tileSize, pixels)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](yearlyData: Iterable[(Int, Data)], generateImage: (Int, Int, Int, Int, Data) => Unit): Unit =
    generateTilesInt(yearlyData, generateImage, zRange = 0 to 3)

  def generateTilesInt[Data](
    yearlyData: Iterable[(Int, Data)],
    generateImage: (Int, Int, Int, Int, Data) => Unit,
    zRange: Range
  ): Unit = {
    yearlyData.foreach { case (year, data) =>
      val iterations = for {
        z <- zRange
        y <- 0 until pow(2, z).toInt
        x <- 0 until pow(2, z).toInt
      } yield (year, z, x, y, data)
      iterations.par.foreach(generateImage.tupled)
    }
  }

}
