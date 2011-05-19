/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package gsd.cdl.formula

import java.io.File
import java.io.FileFilter

object Tests {
  def main(args:Array[String]) = {
     val fs = new File("input/iml/").listFiles(new FileFilter() {
        def accept(pathname:File) = pathname.getName.endsWith("iml")
      })

     for(f <- fs) {
        printFileResults(f)
     }
  }

  def printFileResults (f: File) = {
      print("*** File " + f.getName + ": ")
      try {
        val (variables, expressions, errors) = Main.parseFile(f.getAbsoluteFile.toString)
//          println(a)
          //println(variables)
          //println(expressions)
//          expressions.foreach(exp => {
//              println(exp.getClass)
//            exp match {
//              case GOr(left, right) => {
//                  println(left.asInstanceOf[GEq].left.getClass)
//                  println(left.asInstanceOf[GEq].right.getClass)
//                  println(right.asInstanceOf[GEq].right.getClass)
//                  println(right.asInstanceOf[GEq].right.getClass)
//              }
//              case _ =>
//            }
//          })
          if (errors.size > 0) {
            println("contains " + errors.size + " errors")
//            errors.foreach(println)
          }
          else
          println("contains no errors")
      } catch {
        case e:Exception => println("Error! File: " + f + " couldn't be parsed because of an error: " + e.getMessage)
      }
      println()

  }
}
