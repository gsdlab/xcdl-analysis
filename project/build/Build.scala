import sbt._

class Build(info: ProjectInfo) extends DefaultProject(info) {

	override def mainScalaSourcePath = "src"
	override def testScalaSourcePath = "test"

	override def mainClass = Some("gsd.cdl.formula.Run")

//  lazy val task1: Task[]
}
