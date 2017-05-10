package observatory

import observatory.Visualization.{Loc, _predictTemperature}

import scala.collection.GenIterable

/**
  * 4th milestone: value-added information
  */
object Manipulation {

  type Lat = Int
  type Lon = Int
  type Temp = Double

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Temp)]): (Lat, Lon) => Temp = {
    val temps = temperatures.par.map { case (l, t) => (Loc(l), t) }
    _makeGrid(temps)
  }

  def _makeGrid(temperatures: GenIterable[(Loc, Temp)]): (Lat, Lon) => Temp =
    (lat, lon) => _predictTemperature(temperatures)(Loc(lat, lon))

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temp)]]): (Lat, Lon) => Temp =
    _average(temperaturess.map(_.map { case (l, t) => (Loc(l), t) }))

  def _average(temperaturess: GenIterable[GenIterable[(Loc, Temp)]]): (Lat, Lon) => Temp = {
    def sequence(s: (Double, Int), d: Double): (Double, Int) = (s._1 + d, s._2 + 1)
    def combine(l: (Double, Int), r: (Double, Int)): (Double, Int) = (l._1 + r._1, l._2 + r._2 )
    (lat, lon) => {
      val (sum, cnt) = temperaturess.map(_makeGrid(_)(lat, lon)).aggregate((0.0, 0))(sequence, combine)
      sum / cnt
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A sequence of grids containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temp)], normals: (Lat, Lon) => Double): (Lat, Lon) => Temp =
    _deviation(temperatures.par.map { case (l, t) => (Loc(l), t) }, normals)

  def _deviation(temperatures: GenIterable[(Loc, Temp)], normals: (Lat, Lon) => Double): (Lat, Lon) => Temp =
    (lat, lon) => _makeGrid(temperatures)(lat, lon) - normals(lat, lon)

}

