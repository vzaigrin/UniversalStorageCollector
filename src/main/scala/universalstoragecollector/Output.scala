package universalstoragecollector

abstract class Output(name: String, config: Map[String, String],
                      sysConfig: Map[String, Option[String]] = Map()) {
  def isValid: Boolean
  def start(): Unit
  def out(msg: Map[Int, (String, String)], timestamp: Long, data: Map[String, String]): Unit
  def stop(): Unit
}

object Output {
  def apply(name: String, config: Map[String, String],
            sysConfig: Map[String, Option[String]] = Map()): Output = {
    config.getOrElse("type", "") match {
      case "carbon" => new Carbon("carbon", config, sysConfig)
      case "influxdb" => new Influx("influxdb", config, sysConfig)
      case _ => new Output(name, config, sysConfig) {
                  def isValid = false
                  def start(): Unit = {}
                  def out(msg: Map[Int, (String, String)],
                          timestamp: Long,
                          data: Map[String, String]): Unit = {}
                  def stop(): Unit = {}
                }
    }
  }
}
