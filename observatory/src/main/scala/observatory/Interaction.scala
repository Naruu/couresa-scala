package observatory

import scala.math.{Pi, sinh, atan, pow}
import com.sksamuel.scrimage.{Image, Pixel}

import observatory.Visualization._

/**
  * 3rd milestone: interactive visualization
  */
object Interaction extends InteractionInterface {
  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    val n = pow(2, tile.zoom)
    val lon_deg = tile.y / n * 360.0 - 180.0
    val lat_rad = atan(sinh(Pi * (1 - 2 * tile.x / n)))
    val lat_deg = lat_rad * 180.0 / Pi
    Location(lat_deg, lon_deg)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
      val n = pow(2, 8).toInt
      val coords = for(i <- 0 to 255; j <- 0 to 255) yield (n * tile.x + i, n * tile.y + j)

    val pixels = coords.par
    .map({
      case (y, x) =>
      (x, y, interpolateColor(colors, predictTemperature(temperatures, tileLocation(Tile(y, x, tile.zoom + 8)))))
    }).map({
      case (x,y, color:Color) => (x, y, Pixel(color.red, color.green, color.blue, 127))
    }).toArray.sortBy(x => (x._1, x._2)).map(_._3)

    Image(256,256, pixels)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit
  ): Unit = {
    yearlyData.par
    .foreach({
      case (year, data) => {
        for(zoom <- 0 to 3) {
          val n = pow(2, zoom).toInt
          for(i <- 0 until n; j <- 0 until n) generateImage(year, Tile(i, j, zoom), data)
        }
      }
    })
  }

}