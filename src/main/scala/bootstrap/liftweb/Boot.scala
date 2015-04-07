package bootstrap.liftweb

import java.util.{Date, TimeZone}

import code.model._
import code.pages.MvcController
import code.util.{DBSchema, Migrations}
import code.util.actors.ActorSystemFacade
import net.liftweb.common.{Full, _}
import net.liftweb.http.js.JsCmds
import net.liftweb.http.js.JsCmds.Reload
import net.liftweb.http.js.jquery.JQueryArtifacts
import net.liftweb.http.{Html5Properties, _}
import net.liftweb.util._
import org.joda.time.DateTimeZone
import org.squeryl.{Session, SessionFactory}


class Boot extends Logger {
  def boot(): Unit = try {

    TimeZone.setDefault(TimeZone.getTimeZone("UTC"))

    println("CURRENT SERVER TIME IS: " + new Date().toString)

    DateTimeZone.setDefault(DateTimeZone.UTC)

    LiftRules.resourceNames = "i18n/messages" :: LiftRules.resourceNames

    println("Run mode is " + Props.mode)

    SessionFactory.concreteFactory = Some(() => {
      //TODO: connection pooling:
      Session.create(DBS.conn, new org.squeryl.adapters.PostgreSqlAdapter())
    })

    S.addAround(new LoanWrapper {
      override def apply[T](f: => T): T = {
        val resultOrExcept = {
          try {
            Right(f)
          } catch {
            case e: LiftFlowOfControlException => Left(e)
          }
        }

        resultOrExcept match {
          case Right(result) => result
          case Left(except) => throw except
        }
      }
    })

    DBSchema.main(Array())

    Migrations.run()

    LiftRules.unloadHooks.append(() => {
      println("SHUTTING DOWN HIKARI DATA SOURCE")
      DBS.ds.shutdown()
    })

    Mailer.authenticator = Full(
      new javax.mail.Authenticator() {
        override protected def getPasswordAuthentication: javax.mail.PasswordAuthentication = {
          new javax.mail.PasswordAuthentication(Props.get("email.email").get, Props.get("email.pass").get)
        }
      })

    Mailer.hostFunc = () => Props.get("email.host").get
    Mailer.customProperties = Mailer.customProperties + (("mail.smtp.auth", Props.get("email.smtp.auth").get))
    Mailer.customProperties = Mailer.customProperties + (("mail.smtp.starttls.enable", Props.get("email.smtp.starttls.enable").get))

    LiftRules.addToPackages("code")

    LiftRules.earlyInStateful.prepend((r: Box[Req]) => {
      r.foreach(r => if (r.request != null) {
        User.currentUserOpt(r.cookies.find(_.name == "rememberLogin").map(_.value.openOr("")).getOrElse(""))
      })
    })


    //    LiftRules.dispatch.append {
    //
    ////      case r@Req(path, suffix, _) if !User.loggedIn &&
    ////        path.headOption != Some("login") &&
    ////        path.headOption != Some("register") &&
    ////        path.headOption != Some("ajax_request") &&
    ////        path.headOption != Some("comet_request") =>
    ////
    ////        if (suffix == "html" || suffix == "") {
    ////          S.containerRequest.map(r => r.uri + r.queryString.dmap("")("?" + _)).filter(_ != "/500").foreach(User.loginRedirect.set(_))
    ////        }
    ////
    ////        () => Full(RedirectResponse("/login"))
    //
    //      case r@Req("admin" :: _, _, _) if !User.isAdmin => () => Full(ForbiddenResponse())
    //    }

    //Init the jQuery module, see http://liftweb.net/jquery for more information.
    LiftRules.jsArtifacts = JQueryArtifacts

    LiftRules.ajaxPostTimeout = 25 * 60 * 1000
    LiftRules.ajaxRetryCount = Full(0)

    // Force the request to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    // What is the function to test if a user is logged in?
    LiftRules.loggedInTest = Full(() => User._currentUser.get.isDefined)

    // Use HTML5 for rendering
    LiftRules.htmlProperties.default.set((r: Req) =>
      new Html5Properties(r.userAgent))

    LiftRules.useXhtmlMimeType = false

    LiftRules.ajaxDefaultFailure = Full(() => JsCmds.Reload)
    LiftRules.ajaxRetryCount = Full(0)
    LiftRules.handleUnmappedParameter.default.set(Vendor.apply((_, f) => {
      if (f.startsWith("F")) S.appendJs(Reload)
    }))
    LiftRules.noCometSessionCmd.default.set(Vendor.apply(() => Reload))

    LiftRules.dispatch.append(MvcController)

    ActorSystemFacade.start()

  } catch {
    case e: Exception => e.printStackTrace()
  }
}
