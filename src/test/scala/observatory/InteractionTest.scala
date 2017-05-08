package observatory

import observatory.Extraction.{locateTemperatures, locationYearlyAverageRecords}
import observatory.Interaction.tile
import observatory.Visualization.visualize
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

import scala.collection.concurrent.TrieMap

@RunWith(classOf[JUnitRunner])
class InteractionTest extends FunSuite with Checkers {

  test("tile000") {
    val temps = locateTemperatures(year = 2021, "/stations.csv", "/2021.csv")
    assert(temps.nonEmpty, "No temperatures available")
    val temperatures = locationYearlyAverageRecords(temps)
    val colors = Vector(
      60.0  -> Color(255, 255, 255),
      32.0  -> Color(255, 0,   0  ),
      12.0  -> Color(255, 255, 0  ),
      0.0   -> Color(0,   255, 255),
      -15.0 -> Color(0,   0,   255),
      -27.0 -> Color(255, 0,   255),
      -50.0 -> Color(33,  0,   107),
      -60.0 -> Color(0,   0,   0  )
    )
    tile(temperatures, colors, zoom = 0, x = 0, y = 0).output("/tmp/tile000.png")
    tile(temperatures, colors, zoom = 1, x = 0, y = 0).output("/tmp/tile100.png")
    tile(temperatures, colors, zoom = 1, x = 1, y = 0).output("/tmp/tile110.png")
    tile(temperatures, colors, zoom = 1, x = 0, y = 1).output("/tmp/tile101.png")
    tile(temperatures, colors, zoom = 1, x = 1, y = 1).output("/tmp/tile111.png")
    ()
  }

}
