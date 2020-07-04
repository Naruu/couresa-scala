package observatory

import java.time.LocalDate
import scala.io.Source
import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD

import org.apache.spark.sql.types._

/**
  * 1st milestone: data extraction
  */
object Extraction extends ExtractionInterface {

  @transient lazy val conf: SparkConf = new SparkConf().setMaster("local").setAppName("Extraction")
  @transient lazy val sc: SparkContext = new SparkContext(conf)

case class Key(stn: String, wban: String) extends Serializable
case class Loc(latitude: String, longitude: String) extends Serializable
  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
        
    val stationsLines = Source.fromInputStream(getClass.getResourceAsStream(stationsFile), "utf-8").getLines().toSeq
    val stationsRDD: RDD[(Key, Loc)] = sc.parallelize(stationsLines.map(line => line.split(","))
    .filter(line => (line.size == 4) && line(2).nonEmpty && line(3).nonEmpty)
    .map(arr => (Key(stn = arr(0), wban = arr(1)), Loc(latitude = arr(2), longitude = arr(3)))
    ))

    val temperaturesLines = Source.fromInputStream(getClass.getResourceAsStream(temperaturesFile), "utf-8").getLines().toSeq
    val tempRDD:RDD[(Key, (LocalDate, Temperature))] = sc.parallelize(temperaturesLines.map(line => {
                                                                      val arr = line.split(",")
                                                                      (Key(stn = arr(0), wban = arr(1)),
                                                                      (LocalDate.of(year, arr(2).toInt, arr(3).toInt),
                                                                      (arr(4).toDouble - 32)*5/9)
                                                                      )
                                                                      }))
    val filteredStations = stationsRDD.filter({ case (Key(stn: String, wban: String), Loc(latitude: String, longitude:String)) => latitude.nonEmpty && longitude.nonEmpty })
    .mapValues({case Loc(latitude: String, longitude:String) => Location(lat = latitude.toDouble, lon = longitude.toDouble)})
    filteredStations.join(tempRDD).map({case (k,v) => { val loc = v._1
                                                        val tempv = v._2
                                                        (v._2._1, v._1, v._2._2)
                                                        }}
                                                        ).collect
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    val rdd_records = sc.parallelize(records.toSeq)
    val keyed = rdd_records.map({ case (date: LocalDate, location: Location, temperature: Temperature) => ((location, date.getYear), (temperature, 1))})
    keyed.reduceByKey({ case( x, y ) => (x._1 + y._1, x._2 + y._2)})
    .map(row => {
      val k = row._1
      val v = row._2
      (k._1, v._1/ v._2)
    }).mapValues( x => (x, 1) )
    .reduceByKey({ case( x, y ) => (x._1 + y._1, x._2 + y._2)})
    .map(row => {
      val k = row._1
      val v = row._2
      (k, v._1/ v._2)
    }).collect
  }

  def main(args: Array[String]): Unit = {
    val stationsFile = "/stations_short.csv"
    locateTemperatures(1975,"/stations_short.csv","/1975_short.csv")
  }


}