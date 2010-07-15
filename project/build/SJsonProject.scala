import sbt._

class SJsonProject(info: ProjectInfo) extends DefaultProject(info) with TemplateProject
{
  val scalaToolsSnapshots = "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots"
  val scalaToolsReleases = "Scala-Tools Maven2 Releases Repository" at "http://scala-tools.org/repo-releases"
  val dispatch_json = "net.databinder" % "dispatch-json_2.8.0" % "0.7.4" % "compile"
  val commons_io = "commons-io" % "commons-io" % "1.4" % "compile"
  val objenesis = "org.objenesis" % "objenesis" % "1.2" % "compile"

  val scalatest =
    buildScalaVersion match {
      case "2.7.7" => 
        "org.scalatest" % "scalatest" % "1.0" 
      case "2.8.0.Beta1" =>
        "org.scalatest" % "scalatest" % "1.0.1-for-scala-2.8.0.Beta1-with-test-interfaces-0.3-SNAPSHOT" 
      case "2.8.0.RC2" =>
        "org.scalatest" % "scalatest" % "1.2-for-scala-2.8.0.RC2-SNAPSHOT" % "test"
      case "2.8.0.RC3" =>
        "org.scalatest" % "scalatest" % "1.2-for-scala-2.8.0.RC2-SNAPSHOT" % "test"
      case "2.8.0.RC7" =>
        "org.scalatest" % "scalatest" % "1.2-for-scala-2.8.0.RC7-SNAPSHOT" % "test"
      case "2.8.0" =>
        "org.scalatest" % "scalatest" % "1.2-for-scala-2.8.0.final-SNAPSHOT" % "test"
    }

  val junit = "junit" % "junit" % "4.8.1"
}
