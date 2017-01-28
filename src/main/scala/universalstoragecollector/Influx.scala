package universalstoragecollector

import scala.util.Properties.propOrElse
import java.util.concurrent.TimeUnit

import org.influxdb.{InfluxDB, InfluxDBFactory}
import org.influxdb.InfluxDB.ConsistencyLevel
import org.influxdb.dto.Point.Builder
import org.influxdb.dto.{BatchPoints, Point}

class Influx(name: String, config: Map[String, String],
               sysConfig: Map[String, Option[String]])
  extends Output(name, config, sysConfig) with Logger {

  val loggerFile: String = propOrElse("USC_HOME", "") + "log/collector-error.log"

  val address: Option[String] =
    if (config.contains("address"))
      Some(config("address"))
    else
      None

  val port: Option[Int] =
    if (config.contains("port"))
      Some(config("port").toInt)
    else
      None

  val dbname: Option[String] =
    if (config.contains("dbname"))
      Some(config("dbname"))
    else
      None

  var influxDB: InfluxDB = _

  def isValid: Boolean = config.contains("address") & config.contains("port") &
    config.contains("dbname")

  def start(): Unit = {
    influxDB = InfluxDBFactory.connect("http://" + address.get + ":" + port.get)
  }

  def out(msg: Map[String, Option[String]], data: Map[String, String]): Unit = {

    val batchPoints: BatchPoints = BatchPoints
      .database(dbname.get)
      .retentionPolicy("autogen")
      .consistency(ConsistencyLevel.ALL)
      .build()

    val point: Builder = Point.measurement(msg("objectName").get)
      .time(msg("timestamp").get.toLong, TimeUnit.SECONDS)
      .tag("name", sysConfig("name").get)

    if (sysConfig("class").isDefined) point.tag("type", sysConfig("type").get)
    if (sysConfig("type").isDefined) point.tag("class", sysConfig("class").get)
    if (msg("parentType").isDefined) point.tag("parentType", msg("parentType").get)
    if (msg("parentName").isDefined) point.tag("parentName", msg("parentName").get)
    if (msg("objectType").isDefined) point.tag("objectType", msg("objectType").get)

    data.foreach {p => point.addField(p._1, p._2)}

    batchPoints.point(point.build())
    influxDB.write(batchPoints)
  }

  def stop(): Unit = {
    influxDB.close()
  }
}
