package universalstoragecollector

import fr.janalyse.ssh.SSH

import scala.util.Properties.propOrElse
import scala.xml.Node

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

  // Concrete Storage system parameters
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

  def isValid: Boolean = methods.nonEmpty
  def isSystemValid: Boolean = systemValid

  def ask(): Unit = {
    val timestamp: Long = System.currentTimeMillis / 1000

    methods foreach { method =>
      val stdout: Array[String] =
        SSH.once(address.get, username.get, password.get)(_.execute(method._1)).split("\n")
      val header = stdout.head.split(" ").filter(_ != "")
      val values = stdout.tail

      values foreach { value =>
        val hv = (header zip value.split(" ").filter(_ != "")).toMap

        val msg: Map[Int, (String, String)] =
          if (method._2.getOrElse("head", "") != "")
            Map(1 -> (method._2("head"), hv(method._2("head"))))
        else
            Map(1 -> ("node", "system"))

        if ((hv(method._2.getOrElse("param", "")) != "") &&
          (hv(method._2.getOrElse("data", "")) != "")) {
          val data: Map[String, String] = Map(hv(method._2("param")) -> hv(method._2("data")))
          out.out(msg, timestamp, data)
        }
      }
    }
  }
}
