import sbt._ 
 
class Project(info: ProjectInfo) extends DefaultProject(info) with IdeaProject { 
  val scalaToolsSnapshots = ScalaToolsSnapshots
  val scalatest = "org.scalatest" % "scalatest" % "1.3"
}