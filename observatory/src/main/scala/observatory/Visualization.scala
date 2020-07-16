package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import scala.math.{sin, acos, cos, abs, pow, Pi}

import org.apache.spark.rdd.RDD

import observatory.SparkConfig._

/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface {

  val sc = SparkConfig.sc
  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    
    def calculateDistance(p1: Location, p2: Location): Double = {
      val R: Double = 6378.137
      def calculateSigma(p1: Location, p2: Location): Double = {
        if(p1.lon == p2.lon && p1.lat == p2.lat) 0
        else if(abs(p1.lon) == 180-abs(p2.lon) && p1.lon * p2.lon <= 0 && p1.lat == -p2.lat) Pi
        else {
          val lat1 = Math.toRadians(p1.lat)
          val lat2 = Math.toRadians(p2.lat)
          acos(sin(lat1) * sin(lat2) + cos(lat1) * cos(lat2) * cos(Math.toRadians(abs(p1.lon - p2.lon))))
        }
      }
      R * calculateSigma(p1, p2)
    }
    val dists = temperatures.par.map({case (loc, temp) => (loc, temp, calculateDistance(loc, location))})
    //val tempRDD =  sc.parallelize(temperatures.toSeq)
    //val dists = tempRDD.map({case (loc, temp) => (loc, temp, calculateDistance(loc, location))}).cache
    val closest = dists.minBy({case ( loc, temp, dist) => dist})
    //val closest2 = dists.takeOrdered(1)(Ordering.by[(Location, Double, Double), Double](x => x._3))(0)

    if (closest._3 < 1.0) closest._2
    else {
      val results = dists.map({case (loc, temp, dist) => {
        val w = pow(dist, -2)
        (w * temp, w)
        }})
      val up = results.map(x => x._1).reduce(_ + _)
      val down = results.map(x => x._2).reduce(_ + _)
    up/down
    }

  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    //val twos = points.par.map({case (temperature,color) => (abs(temperature - value), temperature, color)})
    //.takeOrdered(2)(Ordering.by(_._1))
    //val twos = sc.parallelize(points.toSeq).map({case (temperature,color) => (abs(temperature - value), temperature, color)}).takeOrdered(2)(Ordering.by(_._1))
    val smaller = points.par.filter({case (temperature,color) => temperature < value})
    val bigger = points.par.filter({case (temperature,color) => temperature >= value})

    if(!bigger.isEmpty && !smaller.isEmpty) {
      val small = smaller.maxBy({case (temperature,color) => temperature})
      val big = bigger.minBy({case (temperature,color) => temperature})
      val denum = big._1 - small._1
      val x_diff = value - small._1
      Color((small._2.red + x_diff / denum * (big._2.red - small._2.red)).round.toInt,
      (small._2.green + x_diff / denum * (big._2.green - small._2.green)).round.toInt,
      (small._2.blue + x_diff / denum * (big._2.blue - small._2.blue)).round.toInt)
    }
    else if(smaller.isEmpty) bigger.minBy({case (temperature,color) => temperature})._2
    else  smaller.maxBy({case (temperature,color) => temperature})._2
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    
    val coords = for { 
      y <- 0 until 180;
      x <- 0 until 360
    } yield (y, x)

    val pixels = coords.par
    .map({
      case (y, x) =>
      (y, x, interpolateColor(colors, predictTemperature(temperatures, Location(90 - y:Double, x - 180:Double))))
    }).map({
      case (x,y, color:Color) => (x, y, Pixel(color.red, color.green, color.blue, 255))
    }).toArray.sortBy(x => (x._1, x._2)).map(_._3)

    Image(360, 180, pixels)
  }

}