import sbt._
import FileUtilities.{clean, createDirectory}

trait TemplateProject extends DefaultProject with FileTasks
{
  // declares fmpp as a managed dependency.  By declaring it in the private 'fmpp' configuration, it doesn't get published
  val fmppDep = "net.sourceforge.fmpp" % "fmpp" % "0.9.13" % "fmpp"
  val fmppConf = config("fmpp") hide
  def fmppClasspath = configurationClasspath(fmppConf)

  // declare the directory structure for the processed sources
  def srcManaged: Path = "src_managed"
  override def mainScalaSourcePath = srcManaged / "main"
  override def testScalaSourcePath = srcManaged / "test"

  // declare the directory structure for the templates
  def srcRoot: Path = "src" / "main" / "scala"
  def testSrcRoot: Path = "src" / "test" / "scala"
 
  // arguments to fmpp
  def fmppArgs = "--ignore-temporary-files" :: Nil

  // creates a task that invokes fmpp
  def fmppTask(args: => List[String], output: => Path, srcRoot: => Path, sources: PathFinder) = {
    runTask(Some("fmpp.tools.CommandLine"), fmppClasspath,
      "-U" :: "all" :: "-S" :: srcRoot.absolutePath :: "-O" :: output.absolutePath :: args ::: sources.getPaths.toList)
  }

  //  Define template actions and make them run before compilation

  lazy val template = fmppTask(fmppArgs, mainScalaSourcePath, srcRoot, sources(srcRoot))
  lazy val testTemplate = fmppTask(fmppArgs, testScalaSourcePath, testSrcRoot, sources(testSrcRoot))

  override def compileAction = super.compileAction dependsOn(template)
  override def testCompileAction = super.testCompileAction dependsOn(testTemplate)
}
