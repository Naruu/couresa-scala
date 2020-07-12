package observatory

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext

object SparkConfig {
    val conf: SparkConf = new SparkConf().setMaster("local").setAppName("Extraction")
    val sc: SparkContext = new SparkContext(conf)
}
