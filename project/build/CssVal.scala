import sbt.{ProjectInfo, DefaultProject}

class CssVal(info: ProjectInfo) extends DefaultProject(info)
{
  /* There seem to be some version skew issues between sbt and ScalaTest,
   * so we're using scalatest 0.9.5 for now. */
  val scalatest = "org.scalatest" % "scalatest" % "0.9.5" % "test->default"

}
