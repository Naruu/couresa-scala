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
    val grid = new Grid
    val temps = grid.grids.par.map({
      case (y, x) => (y, x, predictTemperature(temperatures, grid.coordToLoc(y,x)))
    }).toArray.sortBy(x => (x._1, x._2)).map(_._3)
    
    (g: GridLocation) => temps((90 - g.lat) * 360 +  (g.lon + 180))
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = {
    
    val getTemps = temperaturess.par.map(makeGrid(_))
    val len = temperaturess.size

    val grid = new Grid
    val temps = grid.grids.par.map({
        case (y, x) => getTemps.par.map(t => t apply GridLocation(90-y, x-180)).reduce(_+_)/len
        }
    )
    (g: GridLocation) => temps((90 - g.lat) * 360 +  (g.lon + 180))

  }

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature = {
    val grid = new Grid
    val temps = grid.grids.par.map({
      case (y, x) => (y, x, predictTemperature(temperatures, grid.coordToLoc(y,x)))
    }).toArray.sortBy(x => (x._1, x._2)).map(_._3)
    
    (g: GridLocation) => temps((90 - g.lat) * 360 +  (g.lon + 180)) - normals(g)
  }


}

