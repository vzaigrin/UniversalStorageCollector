package universalstoragecollector

import scala.xml._

// Template for extractors
abstract class Extractor(name: String, param: Node,
                         sysName: String, sysParam: Node = <node></node>,
                         out: Output = Output("", Map(), Map())) {
  def isValid: Boolean
  def isSystemValid: Boolean
  def ask(): Unit
}

// Known extractors are listed here
object Extractor {
  def apply(name: String, param: Node,
            sysName: String = "", sysParam: Node = <node></node>,
            out: Output = Output("", Map(), Map())): Extractor =
    name.toLowerCase match {
        case "vnxblock" => new VNXBlock("vnxblock", param, sysName, sysParam, out)
        case "vnxfile" => new VNXFile("vnxfile", param, sysName, sysParam, out)
        case "vnxnar" => new VNXNar("vnxnar", param, sysName, sysParam, out)
        case "vplex" => new VPLEX("vplex", param, sysName, sysParam, out)
        case "vmax" => new VMAX("vmax", param, sysName, sysParam, out)
        case "rp" => new RP("rp", param, sysName, sysParam, out)
        case "storwize" => new Storwize("storwize", param, sysName, sysParam, out)
        case _ => new Extractor(name, param, sysName, sysParam, out) {
                    def isValid: Boolean = false
                    def isSystemValid: Boolean = false
                    def ask(): Unit = {}
                  }
    }
}
