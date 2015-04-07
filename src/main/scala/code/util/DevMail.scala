package code.util

import java.util.Timer

import code.model.User
import net.liftweb.util.Mailer._
import net.liftweb.util.{Mailer, Props}

trait KnownException extends Exception

object DevMail {

  object IgnoreException extends KnownException

  val enabled = Props.getBool("liveServer", false)
  val lock = new {}
  val queue = collection.mutable.ListBuffer[(Long, String, String)]()

  def init(): Unit =
    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = if (queue.nonEmpty) {
        print("Flushing email...")
        task.run()
        println("done")
      }
    })
  init()

  def mesg(mesgType: String, s: String) =
    if (enabled) lock.synchronized(queue += ((System.currentTimeMillis(), mesgType, s)))
    else {
      println("Ignoring:\n" + s)
    }

  def ex(ex: Throwable, msg: String = "") = ex match {
    case e: KnownException => println("[KNOWN EX]: " + e.getMessage)
    case _ =>
      ex.printStackTrace()
      val all = Iterator.iterate(ex)(_.getCause).takeWhile(_ != null).toList
      val str =
        all.map(ex => List(s"Exception - $msg:", ex.getMessage) ++ ex.getStackTrace.map(_.toString).toList)
          .reduce(_ ++ List("CAUSED BY:") ++ _)
          .mkString("\n")
      mesg("Exception", str)
  }

  val task = new java.util.TimerTask {
    def run(): Unit = if (queue.nonEmpty) {
      val messages = lock.synchronized {
        try {
          queue.toArray
        } finally {
          queue.clear()
        }
      }
      val body = messages.sortBy(-_._1).map({
        case (time, t, mesg) =>

          import net.liftweb.http.S
          val state =
            s"""PAGE: ${S.uriAndQueryString.getOrElse("???")}
               |USER: ${User.currentUserOpt.map(_.email).getOrElse("???")}
               |""".stripMargin

          s"""===================
                |AT: ${new java.util.Date(time).toGMTString}
                |TYPE: $t
                |${if (S.inStatefulScope_?) state else ""}$mesg
              """.stripMargin
      }).mkString("\n")

      Mailer.blockingSendMail(
        From(Props.get("email.email").get),
        Subject(s"[BaseWebApp@${Props.get("hostname").getOrElse("UNKNOWN")} Dev Mail: " + messages.map(_._2).distinct.sorted.mkString(" ")),
        To(Props.get("dev.email").get),
        PlainMailBodyType(body)
      )
    }
  }
  new Timer().schedule(task, 0, 30 * 60 * 1000)
}
