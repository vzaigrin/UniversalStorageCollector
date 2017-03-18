package universalstoragecollector

import scala.util.Properties._
import scala.xml._
import scala.xml.Utility._
import java.io._
import java.util.Date
import scala.concurrent.duration._
import akka.actor.{ ActorRef, ActorSystem, Props }

object utils {
  def replaceByList(text: String, replaceList: List[(String, String)]): String = {
    if(replaceList.nonEmpty)
      replaceByList(text.replace(replaceList.head._1, replaceList.head._2), replaceList.tail)
    else
      text
  }
}

trait Logger {
  val loggerFile: String

  def log(msg: String) {
    val out = new FileWriter(loggerFile, true)
    try out.write(s"${new Date} $msg\n")
    finally out.close()
  }
}

object collector extends App with Logger {

  val home: String = propOrElse("USC_HOME", "")
  if (home == "") {
    println("Setup USC_HOME to run")
    sys.exit(-1)
  }

  val configFile: String =  home + "/conf/collector.xml"
  if (!new File(configFile).exists) {
    println(s"Error: no configure file $configFile")
    sys.exit(-1)
  }

  val config: Node = try {
    trim(XML.loadFile(configFile))
  } catch {
    case _: Throwable =>
      println(s"Error in processing configuration file $configFile")
      sys.exit(-1)
  }

  val loggerFile: String = home + "/log/collector-error.log"

  // Extractors
  if ((config \ "extractors").length < 1) {
    println(s"Error: no extractors in the file $configFile")
    log(s"Error: no extractors in the file $configFile")
    sys.exit(-1)
  }
  var extractors: Map[String, Node] = ((config \ "extractors").head.child map
    (c => c.attributes("name").toString.toLowerCase -> c)).toMap

  extractors.filterNot(e => Extractor(e._1, e._2).isValid).
    foreach(e => log(s"Incorrect Extractor definition: ${e._1}"))
  extractors = extractors.filter(e => Extractor(e._1, e._2).isValid)

  if ((config \ "outputs").length < 1) {
    println(s"Error: no outputs in the file $configFile")
    log(s"Error: no outputs in the file $configFile")
    sys.exit(-1)
  }

  // Outputs
  var outputs: Map[String, Map[String, String]] =
    ((config \ "outputs").head.child map (c =>
      c.attributes("name").toString.toLowerCase -> (c.child map (ch =>
        ch.label -> ch.text
        )).toMap
      )).toMap

  outputs.filterNot(o => Output(o._1, o._2).isValid).
    foreach(o => log(s"Incorrect Output definition: ${o._1}"))
  outputs = outputs.filter(o => Output(o._1, o._2).isValid)

  if ((config \ "systems").length < 1) {
    println(s"Error: no systems in the file $configFile")
    log(s"Error: no systems in the file $configFile")
    sys.exit(-1)
  }

  // Storage systems
  var systemList: List[Map[String, Any]] =
    (config \ "systems").head.child.toList map (s => (s.child map (c =>
      c.label match {
        case "extractor" => "extractor" -> (c.attributes("name").text, c)
        case _ => c.label -> c.text
      })).toMap)

  systemList.
    filterNot(s => s.contains("name") & s.contains("class") &
      s.contains("interval") & s.contains("extractor") & s.contains("output")).
    filterNot(s => extractors.contains(s("extractor").asInstanceOf[(String, Node)]._1) &
      outputs.contains(s("output").asInstanceOf[String])).
    foreach(s => log(s"Incorrect definition for storage system $s"))

  systemList = systemList.
    filter(s => s.contains("name") & s.contains("class") &
      s.contains("interval") & s.contains("extractor") & s.contains("output")).
    filter(s => extractors.contains(s("extractor").asInstanceOf[(String, Node)]._1) &
      outputs.contains(s("output").asInstanceOf[String]))

  // Actors System
  val akkaSystem = ActorSystem("USC")
  import akkaSystem.dispatcher

  //  Storage systems as List of Actors
  var storageList: List[(Serializable, String, Boolean, ActorRef)] = systemList map {s => {
    val duration: Serializable =
      try
        Duration(s("interval").asInstanceOf[String]).asInstanceOf[FiniteDuration]
      catch {
        case _: Throwable => Null
      }
    val title: String =
      if (s.contains("type"))
        s("name").asInstanceOf[String] + "." + s("type").asInstanceOf[String] +
          "." + s("extractor").asInstanceOf[(String, Any)]._1
      else
        s("name").asInstanceOf[String] + "." +
          s("extractor").asInstanceOf[(String, Any)]._1
    val sysConfig: Map[String, Option[String]] =
      Map("name" -> Some(s("name").asInstanceOf[String]),
          "class" -> Some(s("class").asInstanceOf[String]),
          "type" -> { if (s.contains("type")) Some(s("type").asInstanceOf[String])
                      else None }
      )
    val out: Output = Output(s("output").asInstanceOf[String],
      outputs(s("output").asInstanceOf[String]),
      sysConfig)
    val ext: Extractor =
      Extractor(s("extractor").asInstanceOf[(String, Node)]._1,
                extractors(s("extractor").asInstanceOf[(String, Node)]._1),
                s("name").asInstanceOf[String],
                s("extractor").asInstanceOf[(String, Node)]._2, out)

    (duration, title, ext.isSystemValid,
      akkaSystem.actorOf(Props(classOf[Storage], sysConfig, ext, out), title))
  }}

  storageList.filterNot(_._1 != Null).
    foreach(s => log(s"Incorrect interval ${s._1} for storage system ${s._2}"))
  storageList.filterNot(_._3).
    foreach(s => log(s"Incorrect definition for storage system ${s._2}"))
  storageList = storageList.filter(_._1 != Null).filter(_._3)

  // Let's go!
  val actList = storageList map {s =>
    akkaSystem.scheduler.schedule(Duration("0 milliseconds").asInstanceOf[FiniteDuration],
      s._1.asInstanceOf[FiniteDuration], s._4, "ask")}

}
