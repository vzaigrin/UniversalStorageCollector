package universalstoragecollector

import utils._

import scala.xml._
import fr.janalyse.ssh._
import scala.util.Properties.propOrElse
import scala.util.matching.Regex

class VNXFile(name: String, param: Node, sysName: String, sysParam: Node, out: Output)
  extends Extractor(name, param, sysName, sysParam, out) with Logger {

  val loggerFile: String = propOrElse("USC_HOME", "") + "/log/collector-error.log"
  val arg: Regex = "(#)([A-Za-z]+)".r

  // Common parameters
  val fileCmd: Option[String] =
    if ((param \ "cmd").length > 0)
      Some((param \ "cmd").head.child.mkString)
    else
      None

  val methods: Map[String, (String, String)] =
    if((param \ "methods").length > 0)
      ((param \ "methods").head.child map
        (m => m.attributes("name").toString -> ((m \ "cmd").text, (m \ "type").text))).toMap
    else
      Map()

  val replaceList: List[(String, String)] =
    if ((param \ "replaceList").length == 1)
      ((param \ "replaceList").head.child map (c =>
        (c.attributes("what").toString, c.attributes("value").toString))).toList
    else
      List(("", ""))

  // Concrete Storage system parameters
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

  val cs: Option[String] =
    if ((sysParam \ "cs").length > 0)
      Some((sysParam \ "cs").head.child.text)
    else
      None

  val servers: List[String] =
    if ((sysParam \ "servers").length > 0)
      ((sysParam \ "servers").head.child map (c => c.text)).toList
    else
      List()

  var systemMethods: List[String] =
    if ((sysParam \ "methods").length > 0)
      ((sysParam \ "methods").head.child map (c => c.text)).toList
    else
      List()
  systemMethods = systemMethods.filter(m => methods.contains(m))

  val systemValid: Boolean =
    username.isDefined & password.isDefined & cs.isDefined &
      servers.nonEmpty & systemMethods.nonEmpty

  def isValid: Boolean = fileCmd.isDefined & methods.nonEmpty
  def isSystemValid: Boolean = systemValid

  def ask(): Unit = {
    def f3[A,C](t: (List[A],C)) = t._1.head -> (t._1.last, t._2)
    val timestamp: Long = System.currentTimeMillis / 1000

    servers foreach {server =>
      systemMethods foreach { m =>

        val cmd: String = (fileCmd.get.split(" ") map {
          case arg(_, a2) => a2 match {
            case "server" => server
            case "cmd" => methods(m)._1
            case _ => if ((sysParam \ a2).length > 0)
              (sysParam \ a2).head.child.text
            else
              ""
          }
          case c => c
        }).mkString(" ")

        val result: Array[String] =
          SSH.once(cs.get, username.get, password.get)(_.execute(cmd)).split("\n")

        if (result.length > 1) {
          val rh = result.head
          val value: List[String] = result.last.replace("\"", "").split(",").toList.tail

          methods(m)._2 match {
            case "simple" =>
              val header: List[String] = (rh.replace("\"", "").split(",") map { h =>
                replaceByList(h, replaceList)
              }).toList.tail
              val hv: List[(String, String)] = header map { h => h.replace(" ", "") } zip value
              val msg: Map[String, Option[String]] = Map(
                "parentType" -> None,
                "parentName" -> Some(server),
                "objectType" -> Some(m),
                "objectName" -> None,
                "timestamp" -> Some(timestamp.toString)
              )
              val data: Map[String, String] = (hv map { l => l._1 -> l._2 }).toMap
              out.out(msg, data)

            case "composite" =>
              val header: List[String] = (rh.replace("\"", "").split(",") map { h =>
                replaceByList(h, replaceList)
              }).toList.tail
              val hv: List[(List[String], String)] = header map { h =>
                h.replaceFirst(" ", "|").replaceFirst(" ", ".").replace(" ", "")
                  .split("\\|").toList
              } zip value
              val ghv: Map[String, List[(String, (String, String))]] =
                (hv map f3).groupBy(_._1)
              ghv.keys foreach { k =>
                val msg: Map[String, Option[String]] = Map(
                  "parentType" -> None,
                  "parentName" -> Some(server),
                  "objectType" -> Some(m),
                  "objectName" -> Some(k),
                  "timestamp" -> Some(timestamp.toString)
                )
                val data: Map[String, String] =
                  (ghv(k) map { v => v._2._1 -> v._2._2 }).toMap
                out.out(msg, data)
              }

            case "composite2" =>
              val header: List[String] = (rh.replace("\"", "").split(",") map { h =>
                replaceByList(h, replaceList)
              }).toList.tail

              val hv: List[(List[String], String)] = header map { h =>
                h.replaceFirst(" ", ".").replaceFirst(" ", "|").replace(" ", "")
                  .split("\\|").toList
              } zip value
              val ghv: Map[String, List[(String, (String, String))]] =
                (hv map f3).groupBy(_._1)
              ghv.keys foreach { k =>
                val msg: Map[String, Option[String]] = Map(
                  "parentType" -> None,
                  "parentName" -> Some(server),
                  "objectType" -> Some(m),
                  "objectName" -> Some(k),
                  "timestamp" -> Some(timestamp.toString)
                )
                val data: Map[String, String] =
                  (ghv(k) map { v => v._2._1 -> v._2._2 }).toMap
                out.out(msg, data)
              }

            case "composite3" =>
              val header: List[String] = (rh.replace("\"", "").split(",") map { h =>
                replaceByList(h, replaceList)
              }).toList.tail
              val hv: List[(List[String], String)] = header map { h =>
                h.replaceFirst(" ", ".").replaceFirst(" ", "|").replaceFirst(" ", ".")
                  .replace(" ", "").split("\\|").toList
              } zip value
              val ghv: Map[String, List[(String, (String, String))]] =
                (hv map f3).groupBy(_._1)
              ghv.keys foreach { k =>
                val msg: Map[String, Option[String]] = Map(
                  "parentType" -> None,
                  "parentName" -> Some(server),
                  "objectType" -> Some(m),
                  "objectName" -> Some(k),
                  "timestamp" -> Some(timestamp.toString)
                )
                val data: Map[String, String] =
                  (ghv(k) map { v => v._2._1 -> v._2._2 }).toMap
                out.out(msg, data)
              }

            case _ =>
          }
        } else
          log(s"$sysName: Wrong answer from SSH")
      }
    }
  }
}
