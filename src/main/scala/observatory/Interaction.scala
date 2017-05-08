package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.Visualization.{interpolatePixel, predictTemperaturePar}

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
  def tileLocation(zoom: Int, x: Int, y: Int): Location = {
    import scala.math._
    Location(
      lat = toDegrees(atan(sinh(Pi * (1.0 - 2.0 * y.toDouble / (1 << zoom))))),
      lon = x.toDouble / (1 << zoom) * 360.0 - 180.0
    )
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return A 256×256 image showing the contents of the tile defined by `x`, `y` and `zooms`
    */
  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)],
           zoom: Int, x: Int, y: Int): Image = {
    val z = zoom + 8
    val locations = for {
      ty <- 0 until 256
      tx <- 0 until 256
    } yield tileLocation(z, x * 256 + tx, y * 256 + ty)

    assert(locations.length == 65536, "Wrong number of pixels")

    val temps = temperatures.par
    val colorScale = colors.toArray.sortBy(_._1)
    val pixels = locations
      .par
      .map(predictTemperaturePar(temps))
      .map(interpolatePixel(colorScale, alpha = 127))
      .toArray

    Image(w = 256, h = 256, pixels)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Int, Data)],
    generateImage: (Int, Int, Int, Int, Data) => Unit
  ): Unit = {
    ???
  }

}
