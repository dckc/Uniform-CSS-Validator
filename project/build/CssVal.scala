import sbt.{ProjectInfo, DefaultProject}

class CssVal(info: ProjectInfo) extends DefaultProject(info)
{

  val scalatest = "org.scalatest" % "scalatest" % "0.9.5" % "test->default"

}
