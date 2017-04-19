package observatory

import java.nio.file.Paths

import observatory.Extraction._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSuite, MustMatchers}

@RunWith(classOf[JUnitRunner])
class ExtractionTest extends FunSuite with MustMatchers {

  test("stations") {
    val path = Paths.get(getClass.getResource("/stations.csv").toURI)
    withTime("stations()", stations(path).unsafeRun()) must not be empty
  }

  test("temperatures") {
    val path = Paths.get(getClass.getResource("/1982.csv").toURI)
    withTime("temperatures()", temperatures(path).runLog.unsafeRun()) must not be empty
  }

  test("fahrenheit to celsius") {
    fahrenheitToCelsius(19.4) mustBe -7.000000000000001
  }

  test("locate temperatures") {
    withTime("locateTemperatures()", locateTemperatures(year = 1982, "/stations.csv", "/1982.csv")) must not be empty
  }

  test("locationYearlyAverageRecords") {
    val temps = locateTemperatures(year = 1982, "/stations.csv", "/1982.csv")
    withTime("locationYearlyAverageRecords()", locationYearlyAverageRecords(temps)) must not be empty
  }

  private def withTime[A](banner: String, a: => A): A = {
    val start = System.currentTimeMillis()
    val r = a
    val elapsed = System.currentTimeMillis() - start
    println(s"$banner took " + elapsed + " milliseconds")
    r
  }

}
