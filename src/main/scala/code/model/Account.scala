package code.model

import java.util.{Properties, UUID}
import javax.activation.{DataHandler, FileDataSource}
import javax.mail._
import javax.mail.internet._

import code.util.DevMail
import com.github.david04.liftutils.jacksonxml.JSON
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.dsl

case class SmtpConfig(
                       from: String = "Your Name <youremail@gmail.com>",
                       username: String = "youremail@gmail.com",
                       password: String = "",
                       auth: Boolean = true,
                       enableStarttls: Boolean = true,
                       host: String = "smtp.gmail.com",
                       port: Int = 587
                       )

class Account(
               var accountName: String,
               var secret: String = UUID.randomUUID().toString,
               var customSmtpConfig: Boolean = false,
               var smtpConfig: String = "",
               var hasAdwords: Boolean = true
               ) extends Entity[Account] {

  def smtpConf: SmtpConfig = if (smtpConfig != "") JSON.L.readValue[SmtpConfig](smtpConfig, classOf[SmtpConfig]) else SmtpConfig()
  def smtpConf_=(v: SmtpConfig) = smtpConfig = JSON.L.writeValueAsString(v)

  def table = DBS.accounts

  lazy val usrs: dsl.OneToMany[User] = DBS.account2Users.left(this)

  lazy val user = inTransaction(usrs.where(_.owner === true).single)

  def sendEmail(to: String, subject: String, body: String, attachment: Option[(String, String)] = None, html: Boolean = false): Unit = {
    if (customSmtpConfig) {
      sendEmailCustomSMTP(to, subject, body, attachment, html)
    }
  }

  private def sendEmailCustomSMTP(
                                   to: String,
                                   subject: String,
                                   body: String,
                                   attachment: Option[(String, String)],
                                   html: Boolean
                                   ) = {

    val conf = smtpConf

    val props = new Properties()
    props.put("mail.smtp.auth", conf.auth.toString)
    props.put("mail.smtp.starttls.enable", conf.enableStarttls.toString)
    props.put("mail.smtp.host", conf.host)
    props.put("mail.smtp.port", conf.port.toString)

    val session = Session.getInstance(props,
      new javax.mail.Authenticator() {
        override def getPasswordAuthentication: PasswordAuthentication = new PasswordAuthentication(conf.username, conf.password)
      })

    try {

      val message = new MimeMessage(session) {
        setFrom(new InternetAddress(conf.from))
        setRecipients(Message.RecipientType.TO, to)
        setSubject(subject)

        if (html) {
          setContent(body, "text/html; charset=utf-8")
        } else {
          setText(body)
        }

        attachment.foreach({
          case (file, fileName) =>
            setContent(new MimeMultipart() {
              addBodyPart(new MimeBodyPart() {
                setDataHandler(new DataHandler(new FileDataSource(file)))
                setFileName(fileName)
              })
            })
        })
      }

      Transport.send(message)

    } catch {
      case e: Exception =>
        DevMail.ex(e)
        throw e
    }
  }

}