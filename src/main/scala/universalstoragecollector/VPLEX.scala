package universalstoragecollector

import utils._

import scala.xml._
import fr.janalyse.ssh.SSH
import scala.util.Properties.propOrElse

class VPLEX(name: String, param: Node, sysName: String, sysParam: Node, out: Output)
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

  val monitors: List[Map[String, String]] =
    if ((sysParam \ "monitors").length > 0)
      ((sysParam \ "monitors").head.child map (c => c.attributes.asAttrMap)).toList
    else
      List()

  val systemValid: Boolean = address.isDefined & username.isDefined & password.isDefined &
    monitors.nonEmpty

  def isValid: Boolean = true
  def isSystemValid: Boolean = systemValid

  def ask(): Unit = {
    val format = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss")

    monitors foreach { monitor =>
      val file = monitor("file")
      val director = monitor("director")

      val headCmd: String = Array("head", "-1", file).mkString(" ")
      val header: List[String] =
        (SSH.once(address.get, username.get, password.get)(_.execute(headCmd))
          .split(",") map { h => replaceByList(h, replaceList)
        }).toList.tail

      val dataCmd: String = Array("tail", "-1", file).mkString(" ")
      val value: Array[String] =
        SSH.once(address.get, username.get, password.get)(_.execute(dataCmd)).split(",")

      val timestamp: Long = format.parse(value.head).getTime / 1000
      val hv: List[(String, String)] = header zip value.tail filter (_._2 != "")

      if (hv.nonEmpty) {
        hv foreach { p =>
          val pSplit: Array[String] = p._1.split("\\.")
          val measurement: String = pSplit.head
          val mo: Array[String] = pSplit(1).split(" ")
          if (mo.length > 1) {
            val msg: Map[Int, (String, String)] = Map(
              1 -> ("director", director),
              2 -> ("measurement", measurement),
              3 -> ("object", mo.head)
            )
            val data: Map[String, String] = Map(mo(1) -> p._2)
            out.out(msg, timestamp, data)
          } else {
            val msg: Map[Int, (String, String)] = Map(
              1 -> ("director", director),
              2 -> ("measurement", measurement)
            )
            val data: Map[String, String] = Map(mo.head -> p._2)
            out.out(msg, timestamp, data)
          }
        }
      }
    }
  }
}
