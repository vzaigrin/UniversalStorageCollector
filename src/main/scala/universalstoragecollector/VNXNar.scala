package universalstoragecollector

import scala.sys.process._
import scala.util.matching.Regex
import collection.mutable.ListBuffer
import scala.xml.Utility._
import scala.xml._
import scala.io.{BufferedSource, Source}
import java.io._
import scala.util.Properties.propOrElse

// Extractor for EMC VNX Nar files
class VNXNar(name: String, param: Node, sysName: String, sysParam: Node, out: Output)
  extends Extractor(name, param, sysName, sysParam, out) with Logger {

  val loggerFile: String = propOrElse("USC_HOME", "") + "/log/collector-error.log"

  val arg: Regex = "(#)([A-Za-z]+)".r
  val nar: Regex = "(.*.nar)".r

  // Common parameters
  val baseCmd: Option[String] =
    if ((param \ "baseCmd").length > 0)
      Some((param \ "baseCmd").head.child.mkString)
    else
      None

  val listCmd: Option[String] =
    if ((param \ "listCmd").length > 0)
      Some((param \ "listCmd").head.child.mkString)
    else
      None

  val getCmd: Option[String] =
    if ((param \ "getCmd").length > 0)
      Some((param \ "getCmd").head.child.mkString)
    else
      None

  val dataCmd: Option[String] =
    if ((param \ "dataCmd").length > 0)
      Some((param \ "dataCmd").head.child.mkString)
    else
      None

  val relCmd: Option[String] =
    if ((param \ "relCmd").length > 0)
      Some((param \ "relCmd").head.child.mkString)
    else
      None

  // Directory 'pool' should exist in directory $USC_HOME
  val pool: String = propOrElse("USC_HOME", "") + "/pool"
  val poolValid: Boolean = new File(pool).isDirectory

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

  val scope: Option[String] =
    if ((sysParam \ "scope").length > 0)
      Some((sysParam \ "scope").head.child.text)
    else
      None

  val objectsParamList: List[Map[String, Any]] =
    if ((sysParam \ "objects").length > 0)
      ((sysParam \ "objects").head.child map (m =>
        (m.child map (c =>
          c.label match {
            case "params" => "param" -> (c \\ "param" map (h =>
              ((h \ "@long").text, (h \ "@short").text))).toList
            case _ => c.label -> c.text
          })).toMap
        )).toList
    else
      List()

  val objectsParams: Map[String, Map[String, String]] =
    if (objectsParamList.nonEmpty)
      (objectsParamList map {o => o("type").asInstanceOf[String] ->
        (o("param").asInstanceOf[List[(String, String)]] map
          (p => p._1 -> p._2)).toMap
      }).toMap
    else
      Map()

  val systemValid: Boolean = username.isDefined & password.isDefined &
    scope.isDefined & objectsParamList.nonEmpty

  val cmdParam: Map[String, String] =
    if (systemValid)
      Map("address" -> address.get, "username" -> username.get,
        "password" -> password.get, "scope" -> scope.get)
    else
      Map()

  // Validation by common parameters
  def isValid: Boolean = {
    baseCmd.isDefined & listCmd.isDefined & getCmd.isDefined &
      dataCmd.isDefined & relCmd.isDefined & poolValid
  }
  // Validation by concrete storage system parameters
  def isSystemValid: Boolean = systemValid

  // Extracting data from storage system
  def ask(): Unit = {
    val cmd: Array[String] = s"${baseCmd.get} ${listCmd.get}".split(" ") map {
      case arg(_, a2) => cmdParam.getOrElse(a2, " ")
      case c => c
    }
    // Get the list of Nar files on the storage system
    val stdout = ListBuffer[String]()
    val status: Int = try { (cmd.toSeq run ProcessLogger(stdout append _)).exitValue }
    catch {
      case _: Exception => -1
    }

    if (status == 0) {
      val narList: List[String] = (stdout.tail map (s => s.split(" ").last)).toList
      if (narList.nonEmpty) {
        // Check Pool directory for file with sysName name
        val poolFiles: List[String] = getListOfFiles(pool)

        // Get the name of the last processed Nar file
        val lastNar: String =
          if (poolFiles.nonEmpty & poolFiles.contains(sysName)) {
            val fileLines: List[String] = Source.fromFile(pool ++ "/" ++ sysName).getLines.toList
            if (fileLines.nonEmpty) fileLines.head
            else ""
          } else ""

        // Select Nar file to proceed (we will proceed only one)
        val doNar: String =
          if (lastNar != "") {
            val afterLast: List[String] = narList.filter(_ > lastNar)
            if (afterLast.nonEmpty) afterLast.last
            else "" // no more Nar files to proceed
          } else narList.last

        // Proceed selected Nar file
        if (doNar != "")
          if (proceedNar(doNar) == 0) {
            val file = new File(pool ++ "/" ++ sysName)
            val bw = new BufferedWriter(new FileWriter(file))
            bw.write(doNar)
            bw.close()
          }

      }  else log(s"$sysName: Empty NAR files list")
    }  else log(s"$sysName: Error in run: ${cmd.mkString(" ")}\n$stdout")

  }

  // Get list of files in directory
  def getListOfFiles(dir: String): List[String] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      (d.listFiles.filter(_.isFile) map (_.getName) ).toList
    } else List()
  }

  // Proceed Nar file
  def proceedNar(filename: String): Int = {
    val getCmdParam: Map[String, String] =
      Map("address" -> address.get, "username" -> username.get,
          "password" -> password.get, "scope" -> scope.get,
          "pool" -> pool, "filename" -> filename)

    val getCmdArray: Array[String] = s"${baseCmd.get} ${getCmd.get}".split(" ") map {
      case arg(_, a2) => getCmdParam.getOrElse(a2, " ")
      case c => c
    }

    var stdout = ListBuffer[String]()
    val getStatus: Int = try { (getCmdArray.toSeq run ProcessLogger(stdout append _)).exitValue }
    catch {
      case _: Exception => -1
    }
    if (getStatus == 0) {
      val dataCmdParam: Map[String, String] =
        Map("filename" -> (pool + "/" + filename),
            "dataname" -> (pool + "/" + sysName + ".csv"))

      val dataCmdArray: Array[String] = s"${baseCmd.get} ${dataCmd.get}".split(" ") map {
        case arg(_, a2) => dataCmdParam.getOrElse(a2, " ")
        case c => c
      }

      stdout = ListBuffer[String]()
      val dataStatus: Int = try {
        (dataCmdArray.toSeq run ProcessLogger(stdout append _)).exitValue
      }
      catch {
        case _: Exception => -1
      }

      val relCmdParam: Map[String, String] =
        Map("filename" -> (pool + "/" + filename),
            "relname" -> (pool + "/" + sysName + ".xml"))

      val relCmdArray: Array[String] = s"${baseCmd.get} ${relCmd.get}".split(" ") map {
        case arg(_, a2) => relCmdParam.getOrElse(a2, " ")
        case c => c
      }

      stdout = ListBuffer[String]()
      val relStatus: Int = try {
        (relCmdArray.toSeq run ProcessLogger(stdout append _)).exitValue
      }
      catch {
        case _: Exception => -1
      }

      if (relStatus == 0 & dataStatus == 0) {
        // proceed data extracted from NAR file
        proceedData(dataCmdParam("dataname"), relCmdParam("relname"))
        // delete NAR, data and relations files after proceeding
        new File(pool + "/" + filename).delete()
        new File(dataCmdParam("dataname")).delete()
        new File(relCmdParam("relname")).delete()
      } else return -1
    }
    0
  }

  // Proceed CSV file extracted from NAR file
  def proceedData(dataName: String, relName: String): Unit = {
    val format = new java.text.SimpleDateFormat("MM/dd/yyyy HH:mm:ss")

    val objects: Map[String, Map[Int, (String, String)]] = getObjects(relName)
    if (objects == Map()) {
      log(s"Something wrong with relations file $relName")
      return
    }

    val bufferedSource: BufferedSource = Source.fromFile(dataName)
    val lines: Iterator[String] = bufferedSource.getLines
    val header: Map[String, Int] = lines.take(1).next.split(",").map(_.trim).zipWithIndex.toMap

    lines.foreach { line =>
      val cols: Array[String] = line.split(",").map(_.trim)
      val obj: String = cols(0)
      if (objects.contains(obj)) {
      // get objectType
        val oType: String = objects(obj).filter(_._2._1 == "measurement").values.head._2
        if (objectsParams.contains(oType)) {
          val timestamp: Long = format.parse(cols(1)).getTime / 1000
          val opar: Map[String, String] = objectsParams(oType)
          opar.keys foreach {k =>
            if (header.contains(k))
              if (header(k) < cols.length) {
                val msg: Map[Int, (String, String)] =
                  (objects(obj).keys map (k => k -> objects(obj)(k))).toMap
                val data: Map[String, String] = Map(opar(k) -> cols(header(k)))
                out.out(msg, timestamp, data)
              }
          }
        }
      }
    }
    bufferedSource.close
  }

  // Get the map of objects, its type, name and parent from the relations file
  def getObjects(relName: String): Map[String, Map[Int, (String, String)]] = {
    val relations: Node = try {
      trim(XML.loadFile(relName))
    } catch {
      case _: Throwable =>
        log(s"Error in processing relations file $relName")
        return Map()
    }

    val types: Map[String, String] =
      Map("SP" -> "sp", "RAID Group" -> "rg_pool", "Pool" -> "rg_pool",
          "Public RaidGroup LUN" -> "lun", "Public RaidGroup LUN" -> "lun",
          "Pool Public LUN" -> "lun", "Port" -> "port", "Disk" -> "disk")

    val portPattern: Regex = """Port ([0-9]+) \[\w+; ([0-9A-F:]+) \]""".r
    val lunPattern: Regex = """[a-zA-Z0-9\s_-]+ \[([0-9]+).*\]""".r
    val diskPattern: Regex = "Bus ([0-9]+) Enclosure ([0-9]+) Disk ([0-9]+)".r

    val subSystem: NodeSeq = relations \\ "archivefile" \ "object"

    val objectsList: List[Map[String, Any]] =
      ((subSystem \ "object") map { o =>
        val oType: String = (o \ "@type").text
        val oName: String = (o \ "@name").text
        val dType: String = types.getOrElse(oType, oType.replace(" ", "_"))
        oType match {
          case "SP" => Map("name" -> oName,
                           "object" ->
                             Map(1 -> ("measurement", dType),
                                 2 -> ("name", oName.replace("SP ", ""))))
          case _ => Map("name" -> oName,
                        "object" ->
                          Map(1 -> ("measurement", dType),
                              2 -> ("name", oName.replace(" ", "_"))))
        }
      }).toList ++
      ((subSystem \ "object") flatMap { c =>
        val cType: String = (c \ "@type").text
        val cName: String = (c \ "@name").text
        val parentType: String = types.getOrElse(cType, cType.replace(" ", "_"))
        cType match {
          case "SP" => (c \ "object") map { o =>
            val oType: String = (o \ "@type").text
            val objectType: String = types.getOrElse(oType, oType.replace(" ", "_"))
            val oName: String = (o \ "@name").text
            val parentName: String = cName.replace("SP ", "")
            oType match {
              case "Port" =>
                val objectName: String = oName match {
                  case portPattern(p, _) => parentName + p
                  case _ => oName
                }
                Map("name" -> oName,
                    "object" ->
                      Map(1 -> ("parent", parentType),
                          2 -> ("sp", parentName),
                          3 -> ("measurement", objectType),
                          4 -> ("name", objectName)))
              case _ => Map()
            }
          }
          case "RAID Group" => (c \ "object") map { o =>
            val oType: String = (o \ "@type").text
            val objectType: String = types.getOrElse(oType, oType.replace(" ", "_"))
            val oName: String = (o \ "@name").text
            val parentName: String = cName.replace("Raid Group ", "RG")
            oType match {
              case "Public RaidGroup LUN" =>
                val objectName: String = oName match {
                  case lunPattern(l) => l
                  case _ => oName
                }
                Map("name" -> oName,
                    "object" ->
                      Map(1 -> ("parent", parentType),
                          2 -> ("rg", parentName),
                          3 -> ("measurement", objectType),
                          4 -> ("name", objectName)))
              case "Disk" =>
                val objectName: String = oName match {
                  case diskPattern(b, e, d) => b + "-" + e + "-" + d
                  case _ => oName
                }
                Map("name" -> oName,
                    "object" ->
                      Map(1 -> ("parent", parentType),
                          2 -> ("rg", parentName),
                          3 -> ("measurement", objectType),
                          4 -> ("name", objectName)))
              case _ => Map()
            }
          }
          case "Pool" => (c \ "object") map { o =>
            val oType: String = (o \ "@type").text
            val objectType: String = types.getOrElse(oType, oType.replace(" ", "_"))
            val oName: String = (o \ "@name").text
            val parentName: String = cName.replace(" ", "_")
            oType match {
              case "Pool Public LUN" =>
                val objectName: String = oName match {
                  case lunPattern(l) => l
                  case _ => oName
                }
                Map("name" -> oName,
                    "object" ->
                      Map(1 -> ("parent", parentType),
                          2 -> ("pool", parentName),
                          3 -> ("measurement", objectType),
                          4 -> ("name", objectName)))
              case "Disk" =>
                val objectName: String = oName match {
                  case diskPattern(b, e, d) => b + "-" + e + "-" + d
                  case _ => oName
                }
                Map("name" -> oName,
                    "object" ->
                      Map(1 -> ("parent", parentType),
                          2 -> ("pool", parentName),
                          3 -> ("measurement", objectType),
                          4 -> ("name", objectName)))
              case _ => Map()
            }
          }
          case _ => Map()
        }
      }).toList.filter(_ != Map()).asInstanceOf[List[Map[String, Any]]]

    (objectsList map (o => o("name").asInstanceOf[String] ->
      o("object").asInstanceOf[Map[Int, (String, String)]])).toMap
  }
}
