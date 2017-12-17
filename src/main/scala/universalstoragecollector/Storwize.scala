package universalstoragecollector

import fr.janalyse.ssh.SSH

import scala.util.Properties.propOrElse
import scala.xml.Node

// Extractor for IBM Storwize
class Storwize(name: String, param: Node, sysName: String, sysParam: Node, out: Output)
  extends Extractor(name, param, sysName, sysParam, out) with Logger {

  val loggerFile: String = propOrElse("USC_HOME", "") + "/log/collector-error.log"

  // Common parameters
  val methods: Map[String, Map[String, String]] =
    if((param \ "methods").length > 0)
      ((param \ "methods").head.child map
        (m => ((m \ "cmd").text, (m \ "output").head.attributes.asAttrMap))
        ).toMap
    else
      Map()

  // Concrete storage system parameters
  val address: Option[String] =
    if ((sysParam \ "address").length > 0)
      Some((sysParam \ "address").head.child.text)
    else
      None

  val username: Option[String] =
    if ((sysParam \ "username").length > 0)
      Some((sysParam \ "username").head.child.text)
    else
      None

  val password: Option[String] =
    if ((sysParam \ "password").length > 0)
      Some((sysParam \ "password").head.child.text)
    else
      None

  val systemValid: Boolean = address.isDefined & username.isDefined & password.isDefined

  // Validation by common parameters
  def isValid: Boolean = methods.nonEmpty
  // Validation by concrete storage system parameters
  def isSystemValid: Boolean = systemValid

  // Extracting data from storage system
  def ask(): Unit = {
    val timestamp: Long = System.currentTimeMillis / 1000

    // For each method defined in configuration will do SSH, execute command and proceed output
    methods foreach { method =>
      val stdout: Array[String] =
        SSH.once(address.get, username.get, password.get)(_.execute(method._1)).split("\n")

      if (stdout.length > 1) {
        val header = stdout.head.split(" ").filter(_ != "")
        val values = stdout.tail

        values foreach { value =>
          val hv = (header zip value.split(" ").filter(_ != "")).toMap

          val msg: Map[Int, (String, String)] =
            if (method._2.getOrElse("head", "") != "")
              Map(1 -> ("measurement", hv(method._2("head"))))
            else
              Map(1 -> ("measurement", "system"))

          if ((hv(method._2.getOrElse("param", "")) != "") &&
            (hv(method._2.getOrElse("data", "")) != "")) {
            val data: Map[String, String] = Map(hv(method._2("param")) -> hv(method._2("data")))
            out.out(msg, timestamp, data)
          }
        }
      }  else
        log(s"$sysName: Wrong answer from SSH on method $method")
    }
  }
}
