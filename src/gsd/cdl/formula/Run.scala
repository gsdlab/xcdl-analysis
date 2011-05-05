package gsd.cdl.formula

import gsd.cdl._
import gsd.cdl.model._
import collection._
import org.kiama.rewriting.Rewriter._
import TypeHelper._
import GRewriter._
import ConditionalCompilation._
import java.io._

object Run {

	def main(args:Array[String]) {

    println("main being executed...")

    println("parsing the file...")
		val file = 
//    "/home/marko/NetBeansProjects/ece725-project/input/iml/aaed2000.modified.iml"
    "/home/marko/workspaces/idea/variability/variability/CDLTools/input/iml/pc_vmWare.iml"
    val (vars, constraints, activeConstraints, effectiveConstraints, errors) = 
      Main.convertToGeneralModelFull(EcosIML.parseFile(file))
//    errors.foreach(println)

    //activeConstraints.foreach(keyValue => {println ("" + keyValue._1 + ":" + keyValue._2 )})
    //effectiveConstraints.foreach(keyValue => {println ("" + keyValue._1 + ":" + keyValue._2 )})

		if (errors.size == 0) {
      println("No errors")
    } else {
      println("There were " + errors.size + " errors:")
      errors.foreach(println)
    }
    //constraints.foreach(println)

	}

}
