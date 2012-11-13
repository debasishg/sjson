import sbt._
import Keys._

object SJsonProject extends Build
{
  lazy val root = Project("sjson", file(".")) settings(coreSettings : _*)

  lazy val commonSettings: Seq[Setting[_]] = Seq(
    organization := "net.debasishg",
    version := "0.18",
    scalaVersion := "2.9.2",
    crossScalaVersions := Seq("2.9.2", "2.9.1"),
    scalacOptions ++= Seq("-deprecation", "-unchecked")
  )

  lazy val coreSettings = commonSettings ++ template ++ Seq(
    name := "sjson",
    libraryDependencies ++= Seq("net.databinder" % "dispatch-json_2.9.2" % "0.8.8",
                                "commons-io" % "commons-io" % "1.4",
                                "org.objenesis" % "objenesis" % "1.2",
                                "junit" % "junit" % "4.8.1" % "test",
                                "org.scalatest" % "scalatest_2.9.2" % "2.0.M4" % "test"),
    parallelExecution in Test := false,
    publishTo <<= version { (v: String) => 
      val nexus = "https://oss.sonatype.org/" 
      if (v.trim.endsWith("SNAPSHOT")) Some("snapshots" at nexus + "content/repositories/snapshots")
      else Some("releases" at nexus + "service/local/staging/deploy/maven2") 
    },
    credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := { repo => false },
    pomExtra := (
      <url>https://github.com/debasishg/sjson</url>
      <licenses>
        <license>
          <name>Apache 2.0 License</name>
          <url>http://www.apache.org/licenses/LICENSE-2.0.html</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git@github.com:debasishg/sjson.git</url>
        <connection>scm:git:git@github.com:debasishg/sjson.git</connection>
      </scm>
      <developers>
        <developer>
          <id>debasishg</id>
          <name>Debasish Ghosh</name>
          <url>http://debasishg.blogspot.com</url>
        </developer>
      </developers>),
    unmanagedResources in Compile <+= baseDirectory map { _ / "LICENSE" }
  )

  lazy val fmpp = TaskKey[Seq[File]]("fmpp")
  lazy val fmppOptions = SettingKey[Seq[String]]("fmpp-options")
  lazy val fmppConfig = config("fmpp") hide

  lazy val template = fmppConfig(Test) ++ fmppConfig(Compile) ++ templateBase
  lazy val templateBase = Seq(
    libraryDependencies += "net.sourceforge.fmpp" % "fmpp" % "0.9.14" % fmppConfig.name,
    ivyConfigurations += fmppConfig,
    fmppOptions := "--ignore-temporary-files" :: Nil,
    fullClasspath in fmppConfig <<= update map { _ select configurationFilter(fmppConfig.name) map Attributed.blank }
  )

  def fmppConfig(c: Configuration): Seq[Setting[_]] = inConfig(c)(Seq(
    sourceGenerators <+= fmpp,
    fmpp <<= fmppTask,
    mappings in packageSrc <<= (managedSources, sourceManaged) map { (srcs, base) => srcs x relativeTo(base) },
    sources <<= managedSources
  ))

  lazy val fmppTask =
    (fullClasspath in fmppConfig, runner in fmpp, unmanagedSources, sourceDirectory, includeFilter in unmanagedSources, sourceManaged, fmppOptions, streams) map { (cp, r, sources, srcRoot, filter, output, args, s) =>
      IO.delete(output)
      val arguments = "-U" +: "all" +: "-S" +: srcRoot.getAbsolutePath +: "-O" +: output.getAbsolutePath +: (args ++ sources.getPaths)
      toError(r.run("fmpp.tools.CommandLine", cp.files, arguments, s.log))
      (output ** filter).get
  }
}
