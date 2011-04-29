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
		val file = "/home/marko/tmp/gsd-papers/2011-unknown-attributted-FMs/examples/clafer/extracted_representative_model-cleaned.iml"
		val nodes = EcosIML.parseFile(file)
		//val nodes = EcosIML.parseFile("/home/marko/NetBeansProjects/CDLAnalysis/pc_vmWare-claferPart.iml")
		
    val (vars, constraints, activeConstraints, effectiveConstraints, errors) = Main.convertToGeneralModelFull(nodes)
//								constraints.foreach(println)

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
