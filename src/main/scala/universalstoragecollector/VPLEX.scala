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
    def f3[A,C](t: (List[A],C)) = t._1.head -> (t._1.last, t._2)
    val format = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss")

    monitors foreach {monitor =>
      val file = monitor("file")
      val director = monitor("director")

      val headCmd = Array("head", "-1", file).mkString(" ")
      val header = SSH.once(address.get, username.get, password.get)(_.execute(headCmd)).split(",")

      val dataCmd = Array("tail", "-1", file).mkString(" ")
      val value = SSH.once(address.get, username.get, password.get)(_.execute(dataCmd)).split(",")

      val timestamp: Long = format.parse(value.head).getTime / 1000

      if ((header.length > 1) & (value.length > 1)) {
        val result: List[(List[String], String)] = (header.drop(1) map {h =>
          replaceByList(h, replaceList).replaceFirst(" ", "|").split("\\|").toList
        } zip value.drop(1)).toList
        val ghv: Map[String, List[(String, (String, String))]] = (result map f3).groupBy(_._1)

        ghv.keys foreach {k =>
          if (ghv(k).length > 1) {
            val msg: Map[String, Option[String]] = Map(
              "parentType" -> None,
              "parentName" -> Some(director),
              "objectType" -> None,
              "objectName" -> Some(k),
              "timestamp" -> Some(timestamp.toString)
            )
            ghv(k) foreach { v =>
              val data: Map[String, String] =
                (ghv(k) map {v => v._2._1 -> v._2._2}).toMap
              out.out(msg, data)
            }
          }  else  {
            val msg: Map[String, Option[String]] = Map(
              "parentType" -> None,
              "parentName" -> None,
              "objectType" -> None,
              "objectName" -> Some(director),
              "timestamp" -> Some(timestamp.toString)
            )
            val data: Map[String, String] =
              (ghv(k) map {v => v._2._1 -> v._2._2}).toMap
            out.out(msg, data)
          }
        }
      }
    }
  }
}
