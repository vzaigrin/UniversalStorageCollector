package universalstoragecollector

import java.io.OutputStream
import java.net.Socket
import scala.util.Properties.propOrElse

class Carbon(name: String, config: Map[String, String],
             sysConfig: Map[String, Option[String]])
  extends Output(name, config, sysConfig) with Logger {

  val loggerFile: String = propOrElse("USC_HOME", "") + "/log/collector-error.log"

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

  val title: String = (List("class", "name", "type")
    flatMap (sysConfig.getOrElse(_, None))).mkString(".")

  var sock: Socket = _
  var outStream: OutputStream = _

  def isValid: Boolean = address.isDefined & config.contains("port")

  def start(): Unit = {
    sock = try {
      new Socket(address.get, port.get)
    }
    catch {
      case _: Exception => null
    }
    outStream = if (sock != null) sock.getOutputStream else null
    if (outStream == null) {
      log(s"$title: Can't connect to the carbon server ${address.get}:${port.get}")
      return
    }
  }

  def out(msg: Map[String, Option[String]], data: Map[String, String]): Unit = {

    val head: String = (List("parentType", "parentName", "objectType", "objectName")
      flatMap (m => msg(m))).mkString(".")

    data.foreach { p =>
      try {
        outStream.write(s"$title.$head.${p._1} ${p._2} ${msg("timestamp").get}\r\n".getBytes)
        outStream.flush()
      }  catch {
        case _: Exception => log(s"$title: Error in output to the carbon server ${address.get}:${port.get}")
      }
    }
  }

  def stop(): Unit = {
    if (sock != null) sock.close()
  }
}
