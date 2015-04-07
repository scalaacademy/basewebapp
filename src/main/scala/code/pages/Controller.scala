package code.pages

import net.liftweb.http._

import scala.xml.NodeSeq

object MvcController extends MVCHelper {

//  object Something {
//    def unapply(s: String): Option[code.model.Something] = for {
//      pid <- scala.util.Try(s.toLong).toOption
//      accId <- User.currentAccountOpt.map(_.id)
//      proc <- inTransaction(DBS.somethings.where(p => p.accountId === accId and p.id === pid).headOption)
//    } yield proc
//  }

  def @@(path: Seq[String])(f: NodeSeq => NodeSeq): Option[NodeSeq] = S.session.flatMap(_.findAndProcessTemplate(path.toList).map(f))
  def @@(pathElems: String*)(renderable: {def render: NodeSeq => NodeSeq}): Option[NodeSeq] = @@(pathElems)(renderable.render)

//  serve {
//    case List("list", Something(s)) => @@("pages", "empty")(new SomethingPage()(mc))
//  }
}











