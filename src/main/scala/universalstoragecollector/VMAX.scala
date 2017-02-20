package universalstoragecollector

import utils._
import scala.xml._
import fr.janalyse.ssh.SSH

import scala.util.Properties.propOrElse

class VMAX(name: String, param: Node, sysName: String, sysParam: Node, out: Output)
  extends Extractor(name, param, sysName, sysParam, out) with Logger {

  val loggerFile: String = propOrElse("USC_HOME", "") + "/log/collector-error.log"
  // Common parameters
  val replaceList: List[(String, String)] =
    if ((param \ "replaceList").length == 1)
      ((param \ "replaceList").head.child map (c =>
        (c.attributes("what").toString, c.attributes("value").toString))).toList
    else
      List(("",""))

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

  val sid: Option[String] =
    if ((sysParam \ "sid").length > 0)
      Some((sysParam \ "sid").head.child.text)
    else
      None

  val reports: Option[String] =
    if ((sysParam \ "reports").length > 0)
      Some((sysParam \ "reports").head.child.text)
    else
      None

  val systemValid: Boolean = address.isDefined & username.isDefined & password.isDefined &
    sid.isDefined & reports.isDefined

  def isValid: Boolean = true
  def isSystemValid: Boolean = systemValid

  def ask(): Unit = {
    // get list of reports
    val reportsCmd: String = Array("ls -1 ", reports.get, "/", sid.get, "*").mkString("")
    val reportsList: Array[String] =
      SSH.once(address.get, username.get, password.get)(_.execute(reportsCmd)).split("\n")
    // get name of the last report
    val report: String =  reportsList.last

    // get lines from the report
    val linesCmd: String = Array("cat", report).mkString(" ")
    val lines: Array[String] =
      SSH.once(address.get, username.get, password.get)(_.execute(linesCmd)).split("\n")

    if (lines.length > 0) {
      var header: Array[String] = Array()
      lines.foreach {line =>
        val values: Array[String] = line.split(",")
        if (values(0) == "SYMM_ID") header = values
        else {
          if (header.length > 0) {
            val timestamp: Long = values(2).toLong / 1000
            val msg: Map[Int, (String, String)] = Map(
              1 -> ("measurement", replaceByList(values(4), replaceList)),
              2 -> ("value_type",  replaceByList(values(5), replaceList)),
              3 -> ("instance", replaceByList(values(6), replaceList))
            )
            val data: Map[String, String] =
              (Range(7, values.length) map {i =>
                replaceByList(header(i), replaceList) -> values(i)}).toMap
            out.out(msg, timestamp, data)
          }
        }
      }
    }
  }
}
