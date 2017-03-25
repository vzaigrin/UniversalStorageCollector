package universalstoragecollector

import scala.util.Properties.propOrElse
import scala.util.matching.Regex
import scala.xml.Node
import scalaj.http._
import net.liftweb.json._

// clusters
case class ID(id: Long)
case class Cluster(clusterUID: ID, clusterName: String)
case class ClustersInformation(clustersInformation: List[Cluster])

// clusters/statistics
case class CR(cluster: ID, compressionRatio: Long)
case class OT(cluster: ID, outThroughput: Long)
case class ATS(inThroughput: Long, connectionsOutThroughput: List[OT])
case class ITS(inThroughput: Long, connectionsOutThroughput: List[OT])
case class Traffic(connectionsCompressionRatio: List[CR],
                   applicationThroughputStatistics: ATS,
                   applicationIncomingWrites: Long,
                   initThroughputStatistics: ITS)
case class ClustersStatistics(clusterUID: ID, traffic: Traffic)

// rpas/statistics
case class RPAUID(clusterUID: ID, rpaNumber: Int)
case class RPA(rpaUID: RPAUID, cpuUsage: Int, traffic: Traffic)
case class RPAsStatistics(innerSet: List[RPA])

// groups information
case class CUID(clusterUID: ID, copyUID: Long)
case class CopyUID(groupUID: ID, globalCopyUID: CUID)
case class CI(groupCopyUID: CopyUID, name: String, role: String, vmsInformation: List[String])
case class Group(groupUID: ID, name: String, enabled: Boolean,
                 productionCopiesUID: List[CopyUID], groupCopiesInformation: List[CI])
case class GroupsInfo(innerSet: List[Group])

// groups/statistics
case class CStat(savedSpace: Long, percentageOfSavedSpace: Long)
case class TIM(timeInMicroSeconds: Long)
case class JournalStatistics(distributionFinished: Boolean,
                             imageAccessSizeLeftInBytes: String,
                             imageAccessTimeLeftInSeconds: String,
                             imageAccessTotalSizeInBytes: String,
                             tspUsagePercentage: String,
                             tspUsageInBytes: String,
                             journalLagInBytes: Long,
                             consolidationStatistics: CStat,
                             fastForward: Boolean,
                             movingFrom: TIM,
                             movingTo: TIM,
                             actualJournalSizeInBytes: Long,
                             actualJournalUsageInBytes: Long,
                             usagePercentFromDistributionCapacity: Double)
case class CGCopyStat(copyUID: CopyUID,
                      averageIncomingThroughput: Long, averageIncomingWrites: Long,
                      incomingThroughput: Long, incomingWrites: Long,
                      journalStatistics: JournalStatistics)
case class LinkUID(groupUID: ID, firstCopy: CUID, secondCopy: CUID)
case class InitStat(initCompletionPortion: Long, initIncomingThroughput: Long,
                    initOutgoingThroughput: Long, numberOfGrids: Long)
case class Lag(transactionCounter: Long, timeCounter: Long, dataCounter: Long)
case class TPRPA(rpaUID: RPAUID, numberOfGrids: Long, outgoingThroughput: Long)
case class PipeStat(averageJournalCompressionRatio: Long, compressionRatio: Long,
                    detachData: String, detachDataRate: String, lag: Lag,
                    numberOfGrids: Long, outgoingThroughput: Long,
                    outgoingThroughputPerRPA: List[TPRPA],
                    replicationMode: String, syncRemoteLink: Boolean,
                    deduplicationRatio: Long, averageDeduplicationRatio: Long)
case class CGLinkStat(linkUID: LinkUID, initStatistics: InitStat, pipeStatistics: PipeStat)
case class GroupStat(consistencyGroupUID: ID,
                     consistencyGroupCopyStatistics: List[CGCopyStat],
                     consistencyGroupLinkStatistics: List[CGLinkStat]
                    )
case class GroupsStatistics(innerSet: List[GroupStat])


class RP(name: String, param: Node, sysName: String, sysParam: Node, out: Output)
  extends Extractor(name, param, sysName, sysParam, out) with Logger {

  implicit val formats = DefaultFormats
  val loggerFile: String = propOrElse("USC_HOME", "") + "/log/collector-error.log"
  val arg: Regex = "(#)([A-Za-z]+)".r
  var timestamp: Long = _

  // Common parameters
  val baseUrl: Option[String] =
    if ((param \ "baseUrl").length > 0)
      Some((param \ "baseUrl").head.child.mkString)
    else
      None

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

  val ver: Option[String] =
    if ((sysParam \ "ver").length > 0)
      Some((sysParam \ "ver").head.child.text)
    else
      None

  val systemValid: Boolean =
    address.isDefined & username.isDefined & password.isDefined & ver.isDefined

  val urlParam: Map[String, String] =
    if (systemValid)
      Map("ip" -> address.get, "ver" -> ver.get)
    else
      Map()

  def isValid: Boolean = baseUrl.isDefined
  def isSystemValid: Boolean = systemValid

  def ask(): Unit = {
    timestamp = System.currentTimeMillis / 1000
    var msg: Map[Int, (String, String)] = Map()
    var data: Map[String, String] = Map()

    val base: String = (baseUrl.get.split("/") map {
      case arg(_, a2) => urlParam.getOrElse(a2, "")
      case c => c
    }).mkString("/")

    var clusters: Map[Long, String] = Map()
    var groupsInfo: Map[Long, Map[String, Any]] = Map()

    // clusters
    try {
      val url: String = base + "/clusters"
      val response: HttpResponse[String] =
        Http(url)
          .timeout(connTimeoutMs = 1000, readTimeoutMs = 10000)
          .auth(username.get, password.get)
          .option(HttpOptions.allowUnsafeSSL)
          .asString

      if (response.code == 200)
        clusters =
          (parse(response.body)
            .extract[ClustersInformation]
            .clustersInformation flatMap { c =>
            Map(c.clusterUID.id -> c.clusterName)
          }).toMap
    } catch {
      case e: Exception => log(s"$sysName, /clusters: $e")
    }

    // clusters/statistics
    try {
      clusters foreach { cluster =>
        val url: String =  base + "/clusters/" + cluster._1 + "/statistics"
        val response: HttpResponse[String] =
          Http(url)
            .timeout(connTimeoutMs = 1000, readTimeoutMs = 10000)
            .auth(username.get, password.get)
            .option(HttpOptions.allowUnsafeSSL)
            .asString

        if (response.code == 200) {
          val cs = parse(response.body).extract[ClustersStatistics]
          cs.traffic.connectionsCompressionRatio foreach { c =>
            msg = Map(
              1 -> ("statistics", "clusters"),
              2 -> ("cluster", clusters(cs.clusterUID.id)),
              3 -> ("traffic", "connectionsCompressionRatio"),
              4 -> ("outCluster", clusters(c.cluster.id))
            )
            data = Map("compressionRatio" -> c.compressionRatio.toString)
            out.out(msg, timestamp, data)
          }

          msg = Map(
            1 -> ("statistics", "clusters"),
            2 -> ("cluster", clusters(cs.clusterUID.id)),
            3 -> ("traffic", "applicationThroughputStatistics")
          )
          data =
            Map("inThroughput" -> cs.traffic.applicationThroughputStatistics.inThroughput.toString)
          out.out(msg, timestamp, data)

          cs.traffic.applicationThroughputStatistics.connectionsOutThroughput foreach { c =>
            msg = Map(
              1 -> ("statistics", "clusters"),
              2 -> ("cluster", clusters(cs.clusterUID.id)),
              3 -> ("traffic", "applicationThroughputStatistics"),
              4 -> ("outCluster", clusters(c.cluster.id))
            )
            data = Map("outThroughput" -> c.outThroughput.toString)
            out.out(msg, timestamp, data)
          }

          msg = Map(
            1 -> ("statistics", "clusters"),
            2 -> ("cluster", clusters(cs.clusterUID.id)),
            3 -> ("traffic", "applicationIncomingWrites")
          )
          data =
            Map("applicationIncomingWrites" -> cs.traffic.applicationIncomingWrites.toString)
          out.out(msg, timestamp, data)

          msg = Map(
            1 -> ("statistics", "clusters"),
            2 -> ("cluster", clusters(cs.clusterUID.id)),
            3 -> ("traffic", "initThroughputStatistics")
          )
          data =
            Map("inThroughput" -> cs.traffic.initThroughputStatistics.inThroughput.toString)
          out.out(msg, timestamp, data)

          cs.traffic.initThroughputStatistics.connectionsOutThroughput foreach { c =>
            msg = Map(
              1 -> ("statistics", "clusters"),
              2 -> ("cluster", clusters(cs.clusterUID.id)),
              3 -> ("traffic", "initThroughputStatistics"),
              4 -> ("outCluster", clusters(c.cluster.id))
            )
            data = Map("outThroughput" -> c.outThroughput.toString)
            out.out(msg, timestamp, data)
          }
        }
      }
    } catch {
      case e: Exception => log(s"$sysName, /clusters/statistics: $e")
    }

    // rpas/statistics
    try {
      val url: String = base + "/rpas/statistics"
      val response: HttpResponse[String] =
        Http(url)
          .timeout(connTimeoutMs = 1000, readTimeoutMs = 10000)
          .auth(username.get, password.get)
          .option(HttpOptions.allowUnsafeSSL)
          .asString

      if (response.code == 200) {
        val rpas = parse(response.body).extract[RPAsStatistics].innerSet
        rpas foreach {r =>
          msg = Map(
            1 -> ("statistics", "rpas"),
            2 -> ("cluster", clusters(r.rpaUID.clusterUID.id)),
            3 -> ("rpa",r.rpaUID.rpaNumber.toString)
          )
          data = Map("cpuUsage" -> r.cpuUsage.toString)
          out.out(msg, timestamp, data)

          r.traffic.connectionsCompressionRatio foreach {c =>
            msg = Map(
              1 -> ("statistics", "rpas"),
              2 -> ("cluster", clusters(r.rpaUID.clusterUID.id)),
              3 -> ("rpa",r.rpaUID.rpaNumber.toString),
              4 -> ("traffic", "connectionsCompressionRatio"),
              5 -> ("outCluster", clusters(c.cluster.id))
            )
            data = Map("compressionRatio" -> c.compressionRatio.toString)
            out.out(msg, timestamp, data)
          }

          msg = Map(
            1 -> ("statistics", "rpas"),
            2 -> ("cluster", clusters(r.rpaUID.clusterUID.id)),
            3 -> ("rpa",r.rpaUID.rpaNumber.toString),
            4 -> ("traffic", "applicationThroughputStatistics")
          )
          data =
            Map("inThroughput" -> r.traffic.applicationThroughputStatistics.inThroughput.toString)
          out.out(msg, timestamp, data)

          r.traffic.applicationThroughputStatistics.connectionsOutThroughput foreach {c =>
            msg = Map(
              1 -> ("statistics", "rpas"),
              2 -> ("cluster", clusters(r.rpaUID.clusterUID.id)),
              3 -> ("rpa",r.rpaUID.rpaNumber.toString),
              4 -> ("traffic", "applicationThroughputStatistics"),
              5 -> ("outCluster", clusters(c.cluster.id))
            )
            data = Map("outThroughput" -> c.outThroughput.toString)
            out.out(msg, timestamp, data)
          }

          msg = Map(
            1 -> ("statistics", "rpas"),
            2 -> ("cluster", clusters(r.rpaUID.clusterUID.id)),
            3 -> ("rpa",r.rpaUID.rpaNumber.toString),
            4 -> ("traffic", "applicationIncomingWrites")
          )
          data =
            Map("applicationIncomingWrites" -> r.traffic.applicationIncomingWrites.toString)
          out.out(msg, timestamp, data)

          msg = Map(
            1 -> ("statistics", "rpas"),
            2 -> ("cluster", clusters(r.rpaUID.clusterUID.id)),
            3 -> ("rpa",r.rpaUID.rpaNumber.toString),
            4 -> ("traffic", "initThroughputStatistics")
          )
          data =
            Map("inThroughput" -> r.traffic.initThroughputStatistics.inThroughput.toString)
          out.out(msg, timestamp, data)

          r.traffic.initThroughputStatistics.connectionsOutThroughput foreach {c =>
            msg = Map(
              1 -> ("statistics", "rpas"),
              2 -> ("cluster", clusters(r.rpaUID.clusterUID.id)),
              3 -> ("rpa",r.rpaUID.rpaNumber.toString),
              4 -> ("traffic", "initThroughputStatistics"),
              5 -> ("outCluster", clusters(c.cluster.id))
            )
            data = Map("outThroughput" -> c.outThroughput.toString)
            out.out(msg, timestamp, data)
          }
        }
      }
    } catch {
      case e: Exception => log(s"$sysName, /rpas/statistics: $e")
    }

    // groups information and statistics
    try {
      // groups information
      var url: String = base + "/groups/information"
      var response: HttpResponse[String] =
        Http(url)
          .timeout(connTimeoutMs = 1000, readTimeoutMs = 10000)
          .auth(username.get, password.get)
          .option(HttpOptions.allowUnsafeSSL)
          .asString
      if (response.code == 200) {
        groupsInfo =
          (parse(response.body).extract[GroupsInfo].innerSet map { g =>
            val groupCopies: Map[Long, (String, String)] =
              (g.groupCopiesInformation map { c =>
                (c.groupCopyUID.globalCopyUID.copyUID, (c.name, c.role))
              }).toMap
            g.groupUID.id ->
              Map("name" -> g.name,
                "productionCopiesUID" -> g.productionCopiesUID.head.globalCopyUID.copyUID,
                "groupCopiesInformation" -> groupCopies)
          }).toMap

        // groups/statistics
        url = base + "/groups/statistics"
        response =
          Http(url)
            .timeout(connTimeoutMs = 1000, readTimeoutMs = 10000)
            .auth(username.get, password.get)
            .option(HttpOptions.allowUnsafeSSL)
            .asString

        if (response.code == 200) {
          val gs = parse(response.body).extract[GroupsStatistics].innerSet
          gs foreach { g =>
            g.consistencyGroupCopyStatistics foreach { c =>
              val copyID = c.copyUID.globalCopyUID.copyUID
              val copyInfo =
                groupsInfo(c.copyUID.groupUID.id)("groupCopiesInformation")
                  .asInstanceOf[Map[Long, (String, String)]](copyID)
              msg = Map(
                1 -> ("group", groupsInfo(g.consistencyGroupUID.id)("name").toString),
                2 -> ("statistics", "consistencyGroupCopyStatistics"),
                3 -> ("copy", copyInfo._1.toString)
              )
              data =
                Map("averageIncomingThroughput" -> c.averageIncomingThroughput.toString,
                    "averageIncomingWrites" -> c.averageIncomingWrites.toString,
                    "incomingThroughput" -> c.incomingThroughput.toString,
                    "incomingWrites" -> c.incomingWrites.toString
                )
              out.out(msg, timestamp, data)

              if (copyInfo._2 == "REPLICA") {
                msg = Map(
                  1 -> ("group", groupsInfo(g.consistencyGroupUID.id)("name").toString),
                  2 -> ("statistics", "consistencyGroupCopyStatistics"),
                  3 -> ("copy", copyInfo._1.toString),
                  4 -> ("measurement", "journalStatistics")
                )
                data =
                  Map("averageIncomingThroughput" -> c.journalStatistics.actualJournalSizeInBytes.toString,
                      "actualJournalSizeInBytes" -> c.journalStatistics.actualJournalUsageInBytes.toString,
                      "actualJournalUsageInBytes" -> c.incomingThroughput.toString,
                      "usagePercentFromDistributionCapacity" ->
                        ((math rint c.journalStatistics.usagePercentFromDistributionCapacity * 100) / 100).toString,
                      "journalLagInBytes" -> c.journalStatistics.journalLagInBytes.toString
                  )
                out.out(msg, timestamp, data)
              }
            }

            g.consistencyGroupLinkStatistics foreach { c =>
              val copyInfo =
                groupsInfo(c.linkUID.groupUID.id)("groupCopiesInformation").asInstanceOf[Map[Long, (String, String)]]
              msg = Map(
                1 -> ("group", groupsInfo(g.consistencyGroupUID.id)("name").toString),
                2 -> ("statistics", "consistencyGroupLinkStatistics"),
                3 -> ("firstCopy", copyInfo(c.linkUID.firstCopy.copyUID)._1.toString),
                4 -> ("secondCopy", copyInfo(c.linkUID.secondCopy.copyUID)._1.toString),
                5 -> ("stat", "initStatistics")
              )
              data =
                Map("initCompletionPortion" -> c.initStatistics.initCompletionPortion.toString,
                    "initIncomingThroughput" -> c.initStatistics.initIncomingThroughput.toString,
                    "initOutgoingThroughput" -> c.initStatistics.initOutgoingThroughput.toString,
                    "numberOfGrids" -> c.initStatistics.numberOfGrids.toString
                )
              out.out(msg, timestamp, data)

              msg = Map(
                1 -> ("group", groupsInfo(g.consistencyGroupUID.id)("name").toString),
                2 -> ("statistics", "consistencyGroupLinkStatistics"),
                3 -> ("firstCopy", copyInfo(c.linkUID.firstCopy.copyUID)._1.toString),
                4 -> ("secondCopy", copyInfo(c.linkUID.secondCopy.copyUID)._1.toString),
                5 -> ("stat", "pipeStatistics")
              )
              data =
                Map("averageJournalCompressionRatio" -> c.pipeStatistics.averageJournalCompressionRatio.toString,
                    "compressionRatio" -> c.pipeStatistics.compressionRatio.toString,
                    "numberOfGrids" -> c.pipeStatistics.numberOfGrids.toString,
                    "outgoingThroughput" -> c.pipeStatistics.outgoingThroughput.toString,
                    "deduplicationRatio" -> c.pipeStatistics.deduplicationRatio.toString,
                    "averageDeduplicationRatio" -> c.pipeStatistics.averageDeduplicationRatio.toString
                )
              out.out(msg, timestamp, data)

              if (c.pipeStatistics.detachData != "null") {
                data = Map("detachData" -> c.pipeStatistics.detachData.toString)
                out.out(msg, timestamp, data)
              }
              if (c.pipeStatistics.detachDataRate != "null") {
                data = Map("detachDataRate" -> c.pipeStatistics.detachDataRate.toString)
                out.out(msg, timestamp, data)
              }

              msg = Map(
                1 -> ("group", groupsInfo(g.consistencyGroupUID.id)("name").toString),
                2 -> ("statistics", "consistencyGroupLinkStatistics"),
                3 -> ("firstCopy", copyInfo(c.linkUID.firstCopy.copyUID)._1.toString),
                4 -> ("secondCopy", copyInfo(c.linkUID.secondCopy.copyUID)._1.toString),
                5 -> ("stat", "pipeStatistics"),
                6 -> ("measurement", "lag")
              )
              data =
                Map("transactionCounter" -> c.pipeStatistics.lag.transactionCounter.toString,
                  "timeCounter" -> c.pipeStatistics.lag.timeCounter.toString,
                  "dataCounter" -> c.pipeStatistics.lag.dataCounter.toString
                )
              out.out(msg, timestamp, data)

              c.pipeStatistics.outgoingThroughputPerRPA foreach { r =>
                msg = Map(
                  1 -> ("group", groupsInfo(g.consistencyGroupUID.id)("name").toString),
                  2 -> ("statistics", "consistencyGroupLinkStatistics"),
                  3 -> ("firstCopy", copyInfo(c.linkUID.firstCopy.copyUID)._1.toString),
                  4 -> ("secondCopy", copyInfo(c.linkUID.secondCopy.copyUID)._1.toString),
                  5 -> ("stat", "pipeStatistics"),
                  6 -> ("measurement", "outgoingThroughputPerRPA"),
                  7 -> ("outCluster", clusters(r.rpaUID.clusterUID.id)),
                  8 -> ("outRPA", r.rpaUID.rpaNumber.toString)
                )
                data =
                  Map("numberOfGrids" -> r.numberOfGrids.toString,
                      "outgoingThroughput" -> r.outgoingThroughput.toString
                  )
                out.out(msg, timestamp, data)
              }
            }
          }
        }
      }
    } catch {
      case e: Exception => log(s"$sysName, /groups: $e")
    }
  }
}
