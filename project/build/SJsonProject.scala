import sbt._

class SJsonProject(info: ProjectInfo) extends DefaultProject(info) with TemplateProject
{
  val scalaToolsSnapshots = "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots"
  val scalaToolsReleases = "Scala-Tools Maven2 Releases Repository" at "http://scala-tools.org/repo-releases"
  val dispatch_json = "net.databinder" % "dispatch-json_2.8.1" % "0.7.8" % "compile"
  val commons_io = "commons-io" % "commons-io" % "1.4" % "compile"
  val objenesis = "org.objenesis" % "objenesis" % "1.2" % "compile"

  val scalatest =
    buildScalaVersion match {
      case "2.8.0" =>
        "org.scalatest" % "scalatest" % "1.2" % "test"
      case "2.8.1" =>
        "org.scalatest" % "scalatest" % "1.2" % "test"
    }

  val junit = "junit" % "junit" % "4.8.1"

  override def packageSrcJar = defaultJarPath("-sources.jar")
  lazy val sourceArtifact = Artifact.sources(artifactID)
  override def packageToPublishActions = super.packageToPublishActions ++ Seq(packageSrc)

  override def managedStyle = ManagedStyle.Maven
  Credentials(Path.userHome / ".ivy2" / ".credentials", log)
  lazy val publishTo = "Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/releases/"
//  lazy val publishTo = Resolver.file("Local Test Repository", Path fileProperty "java.io.tmpdir" asFile)
}
