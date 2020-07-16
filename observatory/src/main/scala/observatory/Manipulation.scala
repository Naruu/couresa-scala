package observatory

/**
  * 4th milestone: value-added information
  */
object Manipulation extends ManipulationInterface {
  import observatory.Visualization._

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature = {
    //(g: GridLocation) => predictTemperature(temperatures, Location(g.lat.toDouble, g.lon.toDouble))
    ???
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = {
    /*
    val getTemps = temperaturess.par.map(makeGrid(_))
    (g: GridLocation) => getTemps.map(x => x apply g).reduce(_+_)/getTemps.size
    */
    ???
  }

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature = {
    //val normTemp = (g: GridLocation) => normals(g)
    //val deviation = (g: GridLocation) => makeGrid(temperatures)(g) - normTemp
    ???
  }


}

