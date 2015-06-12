package code.pages

import java.util.UUID

import code.model.{Account, DBS, User}
import code.util.DevMail
import net.liftweb.common._
import net.liftweb.http.js.JE.JsRaw
import net.liftweb.http.js.JsCmds.{Confirm, Alert, RedirectTo, Run}
import net.liftweb.http.{S, SHtml}
import net.liftweb.json.JsonAST.{JString, JArray}
import net.liftweb.json._
import net.liftweb.util.Helpers._
import net.liftweb.util.Mailer._
import net.liftweb.util.{Mailer, Props}
import org.squeryl.PrimitiveTypeMode._

import scala.xml.NodeSeq


class Login {

  def render: NodeSeq => NodeSeq = {

    "@login-btn [onclick]" #> {
      SHtml.jsonCall(
        JsRaw("[$('#email-input').val(), $('#password-input').val()]"),
        (_: JValue) match {
          case JArray(List(JString(email), JString(password))) =>
            if (email == "") Alert("Email is required.")
            else if (password == "") Alert("Password is required.")
            else {
              inTransaction {
                DBS.users.where(_.email === email).headOption.foreach(_.login(password))

                User.currentUserOpt.map(user => {
                  val newToken = UUID.randomUUID().toString
                  user.update(user => user.rememberLogin = newToken)

                  Run(s"document.cookie='rememberLogin=${user.rememberLogin}; path=/;${Props.get("app.cookie.domain").map(domain => s" domain=$domain;").getOrElse("")}'") &
                    //                  RedirectTo(S.param("redir").getOrElse(User.loginRedirect.get))
                    RedirectTo("/")
                }).getOrElse({
                  net.liftweb.http.js.JsCmds.Alert("Invalid login.")
                })
              }
            }
          case _ => Alert("All fields are required.")
        }).cmd
    }
  }
}
