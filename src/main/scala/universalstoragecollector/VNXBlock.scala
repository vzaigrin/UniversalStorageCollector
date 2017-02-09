package universalstoragecollector

import utils._
import sys.process._
import scala.xml._
import collection.mutable.ListBuffer
import scala.util.Properties.propOrElse
import scala.util.matching.Regex

class VNXBlock(name: String, param: Node, sysName: String, sysParam: Node, out: Output)
  extends Extractor(name, param, sysName: String, sysParam, out) with Logger {

  val loggerFile: String = propOrElse("USC_HOME", "") + "/log/collector-error.log"
  val arg: Regex = "(#)([A-Za-z]+)".r
  val numbers: Regex = "[0-9]+".r
  var timestamp: Long = _

  // Common parameters
  val blockCmd: Option[String] =
    if ((param \ "blockCmd").length > 0)
      Some((param \ "blockCmd").head.child.mkString)
    else
      None

  val methods: Map[String, Map[String, Any]] =
    if ((param \ "methods").length > 0)
      ((param \ "methods").head.child map (m =>
        m.attributes("name").toString -> (m.child map (c =>
          c.label match {
            case "paramList" => "params" -> (c \\ "param" map (p => p.text)).toList
            case "headerList" => "header" -> (c \\ "header" map (h =>
              ((h \ "@pos").text, (h \ "@sep").text))).toList
            case _ => c.label -> c.text
          })).toMap
        )).toMap
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

  val scope: Option[String] =
    if ((sysParam \ "scope").length > 0)
      Some((sysParam \ "scope").head.child.text)
    else
      None

  var systemMethods: List[Map[String, String]] =
    if ((sysParam \ "methods").length > 0)
      ((sysParam \ "methods").head.child map (c => c.attributes.asAttrMap)).toList
    else
      List()
  systemMethods = systemMethods.filter(m => methods.contains(m("name")))

  val systemValid: Boolean = username.isDefined & password.isDefined & scope.isDefined &
                             systemMethods.nonEmpty

  val cmdParam: Map[String, String] =
    if (systemValid)
      Map("username" -> username.get, "password" -> password.get, "scope" -> scope.get)
    else
      Map()

  def isValid: Boolean = blockCmd.isDefined & methods.nonEmpty
  def isSystemValid: Boolean = systemValid

  def ask(): Unit = {
    timestamp = System.currentTimeMillis / 1000

    systemMethods foreach {m => {
      val baseCmd: Array[String] = blockCmd.get.split(" ") map {
        case arg(_, a2) => cmdParam.getOrElse(a2, m.getOrElse(a2, " "))
        case c => c
      }
      val cmd: Array[String] = baseCmd ++ methods(m("name"))("cmd").asInstanceOf[String].split(" ")
      val stdout = ListBuffer[String]()
      val status: Int = try { (cmd.toSeq run ProcessLogger(stdout append _)).exitValue }
      catch {
        case e: Exception => log(s"$sysName: Error in run: ${cmd.mkString(" ")}\n${e.getMessage}")
                             -1
      }
      if (status == 0)  {
//        val result: String = stdout mkString "\n"
        methods(m("name"))("type").asInstanceOf[String] match {
          case "simple" => simple(m, stdout)
          case "flat" => flat(m, stdout)
        }
      }   else
        log(s"$sysName: Error in run: ${cmd.mkString(" ")}\n$stdout")
    }}
  }

  def simple(m: Map[String, String], values: ListBuffer[String]): Unit = {
//    val values: Array[String] = result.split("\n")
    val params: List[String] = methods(m("name"))("params").asInstanceOf[List[String]]

    val msg: Map[String, Option[String]] = Map(
      "parentType" -> (if (m.contains("parentType")) Some(m("parentType")) else None),
      "parentName" -> (if (m.contains("parentName")) Some(m("parentName")) else None),
      "objectType" ->
        (if (methods(m("name")).contains("objectType"))
          Some(methods(m("name"))("objectType").asInstanceOf[String])
        else
          None),
      "objectName" -> (if (m.contains("objectName")) Some(m("objectName")) else Some("None")),
      "timestamp" -> Some(timestamp.toString)
    )

    val data: Map[String, String] =
      (values flatMap (v => params map (p => (v, p))))
        .filter(c => c._2.r.findFirstIn(c._1).isDefined)
        .map(r =>
          replaceByList(r._2, replaceList).replace(" ", "") -> r._1.split(" ").last)
//          replaceByList(r._2, replaceList).replace(" ", "") -> numbers.findFirstIn(r._1).mkString)
        .toMap

    out.out(msg, data)
  }

  def flat(m: Map[String, String], values: ListBuffer[String]): Unit = {
//    val values = result.split("\n")
    val params = methods(m("name"))("params").asInstanceOf[List[String]]

    val pm: Regex = methods(m("name"))("pattern").asInstanceOf[String].r
    val header: List[(String, String)] = methods(m("name"))("header")
      .asInstanceOf[List[(String, String)]]

    for(i <- values.indices if pm.findFirstIn(values(i)).isDefined &&
      i + header.length < values.length) {

      val head: String = (values.slice(i,i+header.length).toList zip header map (h =>
        h._2._1 match {
          case "right" => h._1.split(h._2._2)(1)
          case "left" => h._1.split(h._2._2)(0)
          case _ => h._1
        })).mkString

      val msg: Map[String, Option[String]] = Map(
        "parentType" -> (if (m.contains("parentType")) Some(m("parentType")) else None),
        "parentName" -> (if (m.contains("parentName")) Some(m("parentName")) else None),
        "objectType" ->
          (if (methods(m("name")).contains("objectType"))
            Some(methods(m("name"))("objectType").asInstanceOf[String])
          else
            None),
        "objectName" -> Some(replaceByList(head, replaceList).replace(" ","")),
        "timestamp" -> Some(timestamp.toString)
      )

      values.drop(i + header.length).takeWhile(pm.findFirstIn(_).isEmpty) foreach { c =>
        for (p <- params if p.r.findFirstIn(c).isDefined) {
          val data: Map[String, String] =
            (values.drop(i + header.length).takeWhile(pm.findFirstIn(_).isEmpty) flatMap
              (v => params map (p => (v, p))))
              .filter(c => c._2.r.findFirstIn(c._1).isDefined)
              .map(r =>
                replaceByList(r._2, replaceList).replace(" ", "") -> r._1.split(" ").last)
//                  numbers.findFirstIn(r._1).mkString)
              .toMap

          out.out(msg, data)
        }
      }
    }
  }
}
