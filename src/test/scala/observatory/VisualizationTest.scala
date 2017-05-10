package observatory


import java.lang.Math._

import observatory.Extraction.{locateTemperatures, locationYearlyAverageRecords}
import observatory.Visualization._
import org.junit.runner.RunWith
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{FunSuite, MustMatchers}
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class VisualizationTest extends FunSuite with Checkers with MustMatchers {

  implicit val arbLocation: Arbitrary[Loc] = Arbitrary {
    for {
      lat <- Gen.choose(-90.0,  90.0 )
      lon <- Gen.choose(-180.0, 180.0)
    } yield Loc(lat, lon)
  }

  test("greatCircleDistance manual") {
    val x = Loc(-87.82640732750883,-102.39289236101074)
    greatCircleDistance(x, x) must be <= 1.0
  }

  test("greatCircleDistance manual 2") {
    val x = Loc(50.0359, 5.4253)
    val y = Loc(58.3838, 3.0412)
    val expDist = 940900
    val error = abs(greatCircleDistance(x, y) - expDist).toInt
    error must be <= 1000
  }

  test("greatCircleDistance") {
    check((x: Loc) => greatCircleDistance(x, x) <= 0.2)
  }

  test("predict temperature manual") {
    val x = Loc(18.0,-176.0)
    val y = Loc(88.0,-176.0)
    val z = Loc(88.0,-176.0)
    tempDistProp(x, y, z) mustBe true
  }

  test("predict temperature") {
    check((x: Loc, y: Loc, z: Loc) => tempDistProp(x, y, z))
  }

  private def tempDistProp (x: Loc, y: Loc, z: Loc) = {
    val tempX = 10.0
    val tempY = 20.0
    val temperatures = List((x, tempX), (y, tempY))
    val tempZ = predictTemperatureInt(temperatures.par)(z)
    val tempZX = abs(tempX - tempZ)
    val tempZY = abs(tempY - tempZ)
    val zx = greatCircleDistance(z, x)
    val zy = greatCircleDistance(z, y)
    if (abs(zx - zy) <= 0.2) {
      tempZX <= 1.0 && tempZY <= 1.0
    } else {
      if (zx < zy) {
        tempZX < tempZY
      } else {
        tempZX > tempZY
      }
    }
  }

  test("interpolateColor") {
    val scale = List(
      (0.0,           Color(255, 0, 0)),
      (2.147483647E9, Color(0, 0, 255))
    )
    val value = 1.0737418235E9
    val result = interpolateColor(scale, value)
    result mustEqual Color(128, 0, 128)
  }

  test("visualise") {
    val temps = locateTemperatures(year = 1982, "/stations.csv", "/1982.csv")
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
    val image = visualize(temperatures, colors)
    image.output("/tmp/image.png")
    ()
  }
}
