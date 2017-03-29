package universalstoragecollector

import utils._

import scala.xml._
import fr.janalyse.ssh._

import scala.util.Properties.propOrElse
import scala.util.matching.Regex

// Extractor for EMC VNX File
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

  // Concrete storage system parameters
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

  // Validation by common parameters
  def isValid: Boolean = fileCmd.isDefined & methods.nonEmpty
  // Validation by concrete storage system parameters
  def isSystemValid: Boolean = systemValid

  // Extracting data from storage system
  def ask(): Unit = {
    val timestamp: Long = System.currentTimeMillis / 1000

    // For each server on storage system
    servers foreach {server =>
      // proceed methods defined in configuration file
      systemMethods foreach {m =>
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
          // proceed result
          methods(m)._2 match {
            case "simple" =>
              val header: List[String] = (rh.replace("\"", "").split(",") map {h =>
                replaceByList(h, replaceList).replace(" ", "_")
              }).toList.tail
              val hv: List[(String, String)] =
                header map {h => h.replace(" ", "")} zip value filter(_._2 != "")
              val msg: Map[Int, (String, String)] = Map(
                1 -> ("server", server),
                2 -> ("measurement", m)
              )
              val data: Map[String, String] = (hv map {l => l._1 -> l._2}).toMap
              out.out(msg, timestamp, data)

            case "composite" =>
              val header: List[String] = (rh.replace("\"", "").split(",") map {h =>
                replaceByList(h, replaceList)
              }).toList.tail
              val hv: List[(String, String)] = header zip value filter(_._2 != "")

              hv foreach { p =>
                val oName: String = p._1.split(" ").head
                val oValue: String = p._1.split(" ").tail.head
                val param: String = p._1.split(" ").tail.tail.mkString("_")
                val msg: Map[Int, (String, String)] = Map(
                  1 -> ("server", server),
                  2 -> ("measurement", m),
                  3 -> (oName, oValue)
                )
                val data: Map[String, String] = Map(param -> p._2)
                out.out(msg, timestamp, data)
              }

            case "composite2" =>
              val header: List[String] = (rh.replace("\"", "").split(",") map {h =>
                replaceByList(h, replaceList)
              }).toList.tail
              val hv: List[(String, String)] = header zip value filter(_._2 != "")

              hv foreach { p =>
                val oName1: String = p._1.split(" ").head
                val oValue1: String = p._1.split(" ").tail.head
                val oName2: String = p._1.split(" ").tail.tail.head
                val param: String = p._1.split(" ").tail.tail.tail.tail.mkString("_")
                val oValue2: String = p._1.split(" ").tail.tail.tail.head
                val msg: Map[Int, (String, String)] = Map(
                  1 -> ("server", server),
                  2 -> ("measurement", m),
                  3 -> (oName1, oValue1),
                  4 -> (oName2, oValue2)
                )
                val data: Map[String, String] = Map(param -> p._2)
                out.out(msg, timestamp, data)
              }

            case "composite3" =>
              val header: List[String] = (rh.replace("\"", "").split(",") map {h =>
                replaceByList(h, replaceList)
              }).toList.tail
              val hv: List[(String, String)] = header zip value filter(_._2 != "")

              hv foreach { p =>
                val oName1: String = p._1.split(" ").head
                val oValue1: String = p._1.split(" ").tail.head
                val oName2: String = p._1.split(" ").tail.tail.head
                val oValue2: String = p._1.split(" ").tail.tail.tail.head
                val oName3: String = p._1.split(" ").tail.tail.tail.tail.head
                val oValue3: String = p._1.split(" ").tail.tail.tail.tail.tail.head
                val param: String = p._1.split(" ").tail.tail.tail.tail.tail.tail.mkString("_")
                val msg: Map[Int, (String, String)] = Map(
                  1 -> ("server", server),
                  2 -> ("measurement", m),
                  3 -> (oName1, oValue1),
                  4 -> (oName2, oValue2),
                  5 -> (oName3, oValue3)
                )
                val data: Map[String, String] = Map(param -> p._2)
                out.out(msg, timestamp, data)
              }

            case _ =>
          }
        }  else
          log(s"$sysName: Wrong answer from SSH on method $m")
      }
    }
  }
}
