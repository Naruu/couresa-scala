package observatory

import scala.math.pow
import com.sksamuel.scrimage.{Image, Pixel}

import Interaction._
import Visualization._

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 extends Visualization2Interface {

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    point: CellPoint,
    d00: Temperature,
    d01: Temperature,
    d10: Temperature,
    d11: Temperature
  ): Temperature = {
    val x = point.x
    val y = point.y
    d00 * (1-x) * (1 - y) + d10* x * (1-y) + d01 * (1-x) * y + d11 * x * y
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param tile Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: GridLocation => Temperature,
    colors: Iterable[(Temperature, Color)],
    tile: Tile
  ): Image = {
    val n = pow(2, 8).toInt
    val coords = for (i <- 0 until 256 ; j <- 0 until 256) yield (i, j)
    val pixels = coords.par.map({
      case (dy, dx) => tileLocation(Tile(tile.x * n + dy, tile.y + dx, tile.zoom + 8)) })
      .map(interpolateTemp(grid, _))
      .map(interpolateColor(colors, _))
      .map({ case color:Color => Pixel(color.red, color.green, color.blue, 127)
    }).toArray
    Image(256,256, pixels)
  }

  def interpolateTemp(grid: GridLocation => Temperature, loc:Location): Temperature = {
    val lat = loc.lat.toInt
    val lon = loc.lon.toInt

    val p00 = GridLocation(lat, lon)
    val p01 = GridLocation(lat+1, lon)
    val p10 = GridLocation(lat, lon+1)
    val p11 = GridLocation(lat+1, lon+1)
    val point = CellPoint(loc.lat, loc.lon)

    bilinearInterpolation(point, grid(p00), grid(p01), grid(p10), grid(p11))
  }
}
