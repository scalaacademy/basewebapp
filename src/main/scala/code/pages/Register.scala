package code.pages

import java.util.UUID

import code.model.{Account, DBS, User}
import net.liftweb.http.js.JE.JsRaw
import net.liftweb.http.js.JsCmds.{Alert, RedirectTo, Run}
import net.liftweb.http.{S, SHtml}
import net.liftweb.json.JsonAST.{JArray, JString}
import net.liftweb.json._
import net.liftweb.util.Helpers._
import net.liftweb.util.Props
import org.squeryl.PrimitiveTypeMode._

import scala.xml.NodeSeq


class Register {

  def render: NodeSeq => NodeSeq = {

    "@register-btn [onclick]" #> SHtml.jsonCall(
      JsRaw(
        """[
          |  $('#first-name-input').val(),
          |  $('#last-name-input').val(),
          |  $('#email-input').val(),
          |  $('#password1-input').val(),
          |  $('#password2-input').val()
          |]""".stripMargin),
      (_: JValue) match {
        case JArray(List(
        JString(firstName),
        JString(lastName),
        JString(email),
        JString(password1),
        JString(password2)
        )) =>
          if (firstName == "") Alert("First name is required.")
          else if (lastName == "") Alert("Last name is required.")
          else if (email == "") Alert("Email is required.")
          else if (password1 == "") Alert("Password is required.")
          else if (password2 == "") Alert("Password is required.")
          else if (password1 != password2) Alert("Passwords must match.")
          else {
            inTransaction {

              new User(new Account().save().id, firstName, lastName, email).setPassword(password1).save().login(password1)

              User.currentUserOpt.map(user => {
                Run(s"document.cookie='rememberLogin=${user.rememberLogin}; path=/; domain=${Props.get("app.cookie.domain").get}';") &
                  RedirectTo(S.param("redir").getOrElse(User.loginRedirect.get))
              }).getOrElse({
                net.liftweb.http.js.JsCmds.Alert("Invalid login.")
              })
            }
          }

        case _ => Alert("All fields are required.")
      })
  }
}
