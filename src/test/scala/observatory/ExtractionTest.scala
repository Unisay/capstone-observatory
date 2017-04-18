package observatory

import java.nio.file.Paths

import observatory.Extraction.{fahrenheitToCelsius, locateTemperatures, stations, temperatures}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSuite, MustMatchers}

@RunWith(classOf[JUnitRunner])
class ExtractionTest extends FunSuite with MustMatchers {

  test("stations") {
    val path = Paths.get(getClass.getResource("/stations.csv").toURI)

    val start = System.currentTimeMillis()
    val result = stations(path).unsafeRun()
    val elapsed = System.currentTimeMillis() - start

    println("Elapsed: " + elapsed)

    result must not be empty
  }

  test("temperatures") {
    val path = Paths.get(getClass.getResource("/1982.csv").toURI)

    val start = System.currentTimeMillis()
    val result = temperatures(path).runLog.unsafeRun()
    val elapsed = System.currentTimeMillis() - start

    println("Elapsed: " + elapsed)

    result must not be empty
  }

  test("fahrenheit to celsius") {
    fahrenheitToCelsius(19.4) mustBe -7.000000000000001
  }

  test("locate temperatures") {
    val result = locateTemperatures(year = 1982, "/stations.csv", "/1982.csv")
    result must not be empty
  }

}
