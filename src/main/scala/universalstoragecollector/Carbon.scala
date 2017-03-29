package universalstoragecollector

import java.io.OutputStream
import java.net.Socket
import scala.util.Properties.propOrElse

// Output to Carbon (Graphite)
class Carbon(name: String, config: Map[String, String],
             sysConfig: Map[String, Option[String]])
  extends Output(name, config, sysConfig) with Logger {

  val loggerFile: String = propOrElse("USC_HOME", "") + "/log/collector-error.log"

  // Common parameters
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

  // Parameters for concrete storage system
  val title: String = (List("class", "name", "type")
    flatMap (sysConfig.getOrElse(_, None))).mkString(".")

  var sock: Socket = _
  var outStream: OutputStream = _

  // Validation by common parameters
  def isValid: Boolean = address.isDefined & config.contains("port")

  // Connection initialization before data output
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

  // Output data comes in 'msg' and 'data' structures
  def out(msg: Map[Int, (String, String)], timestamp: Long, data: Map[String, String]): Unit = {
    val head: String = (msg.keysIterator.toList.sorted map (msg(_)._2)).mkString(".")
    data.foreach { p =>
      try {
        outStream.write(s"$title.$head.${p._1} ${p._2} $timestamp\r\n".getBytes)
        outStream.flush()
      }  catch {
        case _: Exception => log(s"$title: Error in output to the carbon server ${address.get}:${port.get}")
      }
    }
  }

  // Closing connection after data output
  def stop(): Unit = {
    if (sock != null) sock.close()
  }
}
