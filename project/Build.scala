import sbt._
import Keys._
import com.earldouglas.xsbtwebplugin._
import com.openstudy.sbt.ResourceManagementPlugin._

object Build extends sbt.Build {

  object BaseWebApp {

    val liftVersion = "2.6.1"

    val resolutionRepos = Seq(
      "Typesafe Releases" at "http://repo.typesafe.com/typesafe/maven-releases/",
      "Java.Net" at "http://download.java.net/maven/2/",
      "sonatype" at "https://oss.sonatype.org/content/repositories/releases/",
      "Scala-Tools" at "https://oss.sonatype.org/content/groups/scala-tools/",
      "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
      "releases" at "http://oss.sonatype.org/content/repositories/releases",
      "Restlet" at "http://maven.restlet.org",
      "sonatype-snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
    )

    lazy val projectSettings = Seq(
      unmanagedResourceDirectories in Test <+= (baseDirectory) {_ / "src/main/webapp"},
      watchSources ~= { ws => ws filter (!_.getAbsolutePath.contains("src/main/webapp"))},
      parallelExecution in Test := false,
      unmanagedResourceDirectories in Test <+= (baseDirectory) {_ / "src/main/resources"},
      resolvers ++= resolutionRepos
    )
  }

  lazy val basewebapp = Project("basewebapp", file("."),
    settings =
      Seq(
        Keys.version := "0.1",
        Keys.scalaVersion := "2.11.6"
      ) ++
        Defaults.defaultSettings ++
        WebPlugin.webSettings ++
        BaseWebApp.projectSettings ++
        Seq(
          libraryDependencies ++= Seq(
            "net.liftweb" %% "lift-webkit" % BaseWebApp.liftVersion % "compile" withSources(),
            "com.google.api-ads" % "ads-lib" % "1.33.0",
            "com.google.api-ads" % "adwords-axis" % "1.33.0",
            "com.typesafe.akka" %% "akka-actor" % "2.3.6",
            "javax.servlet" % "javax.servlet-api" % "3.0.1",
            "javax.servlet" % "servlet-api" % "2.5" % "provided",
            "org.eclipse.jetty" % "jetty-webapp" % "9.2.0.M1" % "container",
            "org.eclipse.jetty" % "jetty-plus" % "9.2.0.M1" % "container",
            "ch.qos.logback" % "logback-classic" % "1.0.6",
            "org.postgresql" % "postgresql" % "9.3-1101-jdbc41",
            "org.squeryl" %% "squeryl" % "0.9.5-7",
            "com.zaxxer" % "HikariCP-java6" % "2.2.5" % "compile",
            "com.googlecode.usc" % "jdbcdslog" % "1.0.6.2",
            "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.4.2",
            "com.fasterxml.jackson.datatype" % "jackson-datatype-joda" % "2.4.2",
            "com.github.nscala-time" %% "nscala-time" % "1.2.0",
            //
            "com.google.apis" % "google-api-services-oauth2" % "v2-rev81-1.19.0",
            "com.google.apis" % "google-api-services-plus" % "v1-rev193-1.19.0",
            "com.google.apis" % "google-api-services-gmail" % "v1-rev15-1.19.0",
            "com.google.api-client" % "google-api-client" % "1.19.0",
            "com.google.api-client" % "google-api-client-java6" % "1.19.0",
            //
            // LIFT:
            "commons-codec" % "commons-codec" % "1.6",
            "org.apache.commons" % "commons-math3" % "3.0",
            "commons-fileupload" % "commons-fileupload" % "1.2.2",
            "commons-httpclient" % "commons-httpclient" % "3.1",
            "javax.mail" % "mail" % "1.4.4",
            "org.joda" % "joda-convert" % "1.2",
            "nu.validator.htmlparser" % "htmlparser" % "1.4",
            "org.mongodb" % "mongo-java-driver" % "2.12.2",
            "com.thoughtworks.paranamer" % "paranamer" % "2.4.1",
            "org.scala-libs" %% "scalajpa" % "1.5",
            "org.scalaz" %% "scalaz-core" % "7.1.0",
            "org.slf4j" % "slf4j-api" % "1.7.2",
            "org.scala-lang.modules" %% "scala-xml" % "1.0.1",
            "org.mozilla" % "rhino" % "1.7R4",
            "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1",
            "log4j" % "log4j" % "1.2.16",
            //
            "org.apache.commons" % "commons-lang3" % "3.3.2",
            "com.scalarx" %% "scalarx" % "0.2.6",
            "org.json4s" %% "json4s-native" % "3.2.10",
            "joda-time" % "joda-time" % "2.3",
            "com.github.tototoshi" %% "scala-csv" % "1.1.2",
            "com.github.kevinsawicki" % "timeago" % "1.0.1"
          )
        )
  )
    .settings(parallelExecution := false)
}
