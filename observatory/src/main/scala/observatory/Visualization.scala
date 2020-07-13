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
        else if(abs(p1.lon) == 180-abs(p2.lon) && p1.lon * p2.lon < 0 && p1.lat == -p2.lat) Pi
        else {
          val lat1 = 180 / Pi * p1.lat
          val lat2 = 180 / Pi * p2.lat
          acos(sin(lat1) * sin(lat2) + cos(lat1) * cos(lat2) * cos(abs(p1.lon - p2.lon) * 180 / Pi))
        }
      }
      R * calculateSigma(p1, p2)
    }
    val closest = temperatures.par.minBy{
      case (loc, temp) => calculateDistance(loc, location)
    }
    if (calculateDistance(closest._1, location) <= 1.0) closest._2
    else {
    val tempRDD =  sc.parallelize(temperatures.toSeq)
    val dists = tempRDD.map({case (loc, temp) => (loc, temp, calculateDistance(loc, location))}).cache
      val results = dists.map({case (loc, temp, dist) => {
        val w = pow(dist, -2)
        (w, w * temp)
        }}).cache
      val numerator = results.map(x => x._2).reduce(_ + _)
      val denominator = results.map(x => x._1).reduce(_ +_)
    numerator/denominator
    }
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    val twos = sc.parallelize(points.toSeq).map({case (temperature,color) => (abs(temperature - value), temperature, color)}).takeOrdered(2)(Ordering.by(_._1))
    val small = if(twos(0)._2 < twos(1)._2) twos(0) else twos(1)
    val big = if(twos(0)._2 < twos(1)._2) twos(1)  else twos(0)

    if(value >= big._2) big._3
    else if(value < small._2) small._3
    else {
    val denum = big._2 - small._2
    val x_diff = value - small._2
    Color(((small._3.red).toDouble + x_diff / denum * (big._3.red - small._3.red).toDouble).round.toInt,
    ((small._3.green).toDouble + x_diff / denum * (big._3.green - small._3.green).toDouble).round.toInt,
    ((small._3.blue).toDouble + x_diff / denum * (big._3.blue - small._3.blue).toDouble).round.toInt)
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    /*
    val coords = for { 
      lon <- 0 to 360
      lat <- 0 to 180
    } yield (lat-90:Double, lon-180:Double)

    val pixels = coords.par
    .map({
      case (x,y) =>
      predictTemperature(temperatures, Location(x,y))
    })
    .map(interpolateColor(colors, _))
    .map({
      case color:Color => Pixel(color.red, color.green, color.blue, 255)
    })

    Image(360, 180, pixels.toArray)
    */
    ???
  }

}

