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
    System.getProperty("user.dir") + "/input/iml/cme555.iml"
//    val (enums, vars, constraints, errors) = 
//      Main.parseFileWithEnums(file)


   val info = Main.getApproximatedTypesCount(file)
   println("*** INFO: " + info.getBoolCount);
//    errors.foreach(println)
   println("Parsing file: " + file)
    
    println("\n" + "printing active constraints ...")
//    activeConstraints.foreach(keyValue => {println ("" + keyValue._1 + ":" + keyValue._2 )})
    println("\n" + "printing effective constraints ...")
//    effectiveConstraints.foreach(keyValue => {println ("" + keyValue._1 + ":" + keyValue._2 )})
    println("\n" + "printing constraints ...")
//    constraints.foreach(constr => {println("----"); println(constr)})
    println("\n" + "printing variables ...")
//    vars.foreach(println)
/*
		if (errors.size == 0) {
      println("No errors")
    } else {
      println("There were " + errors.size + " errors:")
//      errors.foreach(println)
    }
    //constraints.foreach(println)
*/
	}
}
