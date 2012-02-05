import sbt._
import Keys._

object SJsonProject extends Build
{
  lazy val root = Project("sjson", file(".")) settings(coreSettings : _*)

  lazy val commonSettings: Seq[Setting[_]] = Seq(
    organization := "net.debasishg",
    version := "0.15",
    scalaVersion := "2.9.1"
  )

  lazy val coreSettings = commonSettings ++ template ++ Seq(
    name := "sjson",
    libraryDependencies ++= Seq("net.databinder" % "dispatch-json_2.9.1" % "0.8.5",
                                "commons-io" % "commons-io" % "1.4",
                                "org.objenesis" % "objenesis" % "1.2",
                                "junit" % "junit" % "4.8.1" % "test",
                                "org.scalatest" % "scalatest_2.9.1" % "1.6.1" % "test"),
    parallelExecution in Test := false,
    publishTo := Some("Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/releases/"),
    credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),
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
