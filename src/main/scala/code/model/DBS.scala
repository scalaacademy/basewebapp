package code.model

import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import net.liftweb.util.Props
import org.jdbcdslog.ConnectionLoggingProxy
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.internals.FieldMetaData
import org.squeryl.{Schema, Table}

/**
 * DB Schema
 */
object DBS extends Schema {

  Class.forName("org.postgresql.Driver")
  Class.forName("org.postgresql.ds.PGSimpleDataSource")

  def toTableName(c: Class[_]) = tableNameFromClassName(c.getName.reverse.takeWhile(_ != '.').reverse)

  val config = new HikariConfig()
  println("MaximumPoolSize: " + Props.get("db.max-conn").get.toInt)
  config.setMaximumPoolSize(Props.get("db.max-conn").get.toInt)
  config.setConnectionTimeout(Props.get("db.conn-timeout").get.toLong)
  config.setDataSourceClassName("org.postgresql.ds.PGSimpleDataSource")
  config.addDataSourceProperty("databaseName", Props.get("db.db").get)
  config.addDataSourceProperty("serverName", Props.get("db.host").get)
  config.addDataSourceProperty("user", Props.get("db.user").get)
  config.addDataSourceProperty("password", Props.get("db.password").get)
  //  config.setUseInstrumentation(false)

  println("CREATING HIKARI DATA SOURCE")
  val ds = new HikariDataSource(config)

  def conn = ConnectionLoggingProxy.wrap(ds.getConnection())

  val sys = table[Sys]

  val accounts = table[Account]
  val users = table[User]

  val account2Users = oneToManyRelation(accounts, users).via((a, u) => a.id === u.accountId)

  override def columnNameFromPropertyName(propertyName: String) = NamingConventionTransforms.snakify(propertyName)

  override def tableNameFromClassName(className: String) = NamingConventionTransforms.snakify(className)

  override def columnTypeFor(fieldMetaData: FieldMetaData, owner: Table[_]) =
    if (fieldMetaData.wrappedFieldType.getSimpleName == "String") Some("text") else None
}
