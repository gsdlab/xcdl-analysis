/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package gsd.cdl.formula

import java.io.File
import java.io.FileFilter

object Tests {
  
  def main(args:Array[String]) = {
     val out = new java.io.FileWriter("typeInference.csv")

     val fs = new File("input/iml/").listFiles(new FileFilter() {
        def accept(pathname:File) = pathname.getName.endsWith(".iml")
     })

     out.write("filename;strings;ints;bools;enums;nones;packages;total\n")

     for(f <- fs) {
        printFileResults(f, out)
     }
     out.close()
  }

  def printFileResults (f: File, out:java.io.FileWriter) = {
      print("*** File " + f.getName + ": ")
      try {
      val info = Main.getApproximatedTypesCount(f.getAbsolutePath)
      out.write(f.getName + ";" + info.getStringCount + ";"  + info.getIntCount + 
      ";" + info.getBoolCount + ";" + info.getEnumCount + 
      ";" + info.getNoneCount + ";" + info.getPackageCount + 
      ";" + info.getTotalCount +
      "\n")
      gsd.cdl.formula.TypeGraph.clear()
/*
        val (enums, variables, expressions, errors) = Main.parseFileWithEnums(f.getAbsoluteFile.toString)
          enums.foreach(pair => {
           pair._2.foreach(v => {
            val value = v.asInstanceOf[GEnumLiteral].originalValue
            if (value.trim != "") {
              try {              
               java.lang.Integer.parseInt(value.trim())
              } catch {
               case _ => out.write("'" + f + "';'" + pair._1 + "';'StringLiteral(" + value + ")'\n")
              }
            }
           })
          })
          if (errors.size > 0) {
            println("contains " + errors.size + " errors")
          }
          else
          println("contains no errors")
*/
      } catch {
        case e:Exception => println("Error! File: " + f + " couldn't be parsed because of an error: " + e.getMessage)
      }
      println()
      out.flush
  }
}
