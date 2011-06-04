/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package gsd.cdl.formula

import java.io.File
import java.io.FileFilter
import gsd.cdl.parser.EcosIML

object EnumerationsByType {
  
  def main(args:Array[String]) = {
     val out = new java.io.FileWriter("highlevel_enumerationByType.csv")

     val fs = new File("input/iml/").listFiles(new FileFilter() {
        def accept(pathname:File) = pathname.getName.endsWith(".iml")
     })

     out.write("filename;int_enums;string_enums;mixed_enums\n")

     for(f <- fs) {
        printFileResults(f, out)
     }
     out.close()
  }

  def printFileResults (f: File, out:java.io.FileWriter) = {
      print("*** File " + f.getName + "\n")
      gsd.cdl.formula.TypeGraph.clear()

      val enums = Main.produceEnumerations(EcosIML.parseFile(f.getAbsolutePath))

      var intNumber = 0
      var stringNumber = 0
      var mixedNumber = 0

      enums.foreach(value => {
        if (value._2.enumType == EnumerationTypes.INTEGERS) {
         intNumber += 1
        } else if (value._2.enumType == EnumerationTypes.STRINGS) {
         stringNumber += 1
        } else {
         mixedNumber += 1
        }
      })
      
      out.write(f.getName + ";" + intNumber + ";" + stringNumber + ";"  + mixedNumber + "\n")

      out.flush
  }
}
