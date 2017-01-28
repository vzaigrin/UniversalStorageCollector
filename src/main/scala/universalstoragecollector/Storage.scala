package universalstoragecollector

import akka.actor.Actor

class Storage(sysConfig: Map[String, Option[String]], ext: Extractor, out: Output)
  extends Actor {
  def receive: PartialFunction[Any, Unit] = {
    case "ask" =>
                  out.start()
                  ext.ask()
                  out.stop()
    case _ =>
  }
}

object Storage {
  def apply(sysConfig: Map[String, Option[String]], ext: Extractor, out: Output): Storage = {
    new Storage(sysConfig, ext, out)
  }
}
