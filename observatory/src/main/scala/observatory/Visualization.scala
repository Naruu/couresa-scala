package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import scala.math.{sin, acos, cos, abs, pow, Pi}

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD

/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface {

  val R: Double = 6371

  @transient lazy val conf: SparkConf = new SparkConf().setMaster("local").setAppName("Visualization")
  @transient lazy val sc: SparkContext = new SparkContext(conf)

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    
    def calculateDistance(p1: Location, p2: Location): Double = {
      def calculateSigma(p1: Location, p2: Location): Double = {
        if(p1.lon == p2.lon && p1.lat == p2.lat) 0
        else if(p1.lon == -p2.lon && p1.lat == -p2.lat) Pi
        else cos(sin(p1.lat) * sin(p2.lat) + cos(p1.lat) * cos(p2.lat) * cos(abs(p1.lon - p2.lon)))
      }
      R * calculateSigma(p1, p2)
    }

    val tempRDD = sc.parallelize(temperatures.toSeq)
    val dists = tempRDD.map({case (loc, temp) => (loc, temp, calculateDistance(loc, location))}).cache
    val close = dists.filter({case (loc, temp, dist) => dist <= 1.0})
    
    if(close.count > 0) close.min()(Ordering.by[(Location, Double, Double), Double](_._3))._2
    else {
      val results = dists.map({case (loc, temp, dist) => {
        val w = 1/pow(dist, 2)
        (w, w*temp)
        }}).cache
      val denominator = results.map(x => x._1).reduce(_ +_)
      val numerator = results.map(x => x._2).reduce(_ + _)
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

    val denum = big._2 - small._2
    val x_diff = value - small._2
    Color(((small._3.red).toDouble + x_diff / denum * (big._3.red - small._3.red).toDouble).round.toInt,
    ((small._3.green).toDouble + x_diff / denum * (big._3.green - small._3.green).toDouble).round.toInt,
    ((small._3.blue).toDouble + x_diff / denum * (big._3.blue - small._3.blue).toDouble).round.toInt)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    ???
  }

}

