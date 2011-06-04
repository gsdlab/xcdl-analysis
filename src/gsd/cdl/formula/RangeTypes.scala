/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package gsd.cdl.formula

import java.io.File
import java.io.FileFilter
import gsd.cdl.parser.EcosIML
import gsd.cdl.model._
import org.kiama.rewriting.Rewriter._

object RangeTypes {
  
  def main(args:Array[String]) = {
     val out = new java.io.FileWriter("highlevel_ranges.csv")

     val fs = new File("input/iml/").listFiles(new FileFilter() {
        def accept(pathname:File) = pathname.getName.endsWith(".iml")
     })

     out.write("filename;min;max\n")

     for(f <- fs) {
        printFileResults(f, out)
     }
     out.close()
  }

  def printFileResults (f: File, out:java.io.FileWriter) = {
      print("*** File " + f.getName + "\n")
      gsd.cdl.formula.TypeGraph.clear()

      val allNodes:List[Node] = collectl { case n:Node => n }(EcosIML.parseFile(f.getAbsolutePath)).map(_ match {
        case Node(id, PackageType, display, description, flavor, defaultValues, calculated, legalValues, reqs, activeIfs, implements, children) => Node(id, PackageType, display, description, NoneFlavor, defaultValues, calculated, legalValues, reqs, activeIfs, implements, children)
        case x@_ => x
      })
    
      var intNumber = 0

      allNodes.foreach(node => {
       node.legalValues match {
        case Some(legalValues) => {
         legalValues.ranges.foreach (value => {
          var minMax = 0
          var single = 0
          var minValueInRange:Long = 0
          var maxValueInRange:Long = 0
          value match {
           case MinMaxRange(min, max) => {
            minMax += 1
            try{
            minValueInRange = min.asInstanceOf[LongIntLiteral].value
            maxValueInRange = max.asInstanceOf[LongIntLiteral].value
            out.write(f.getName + ";" + minValueInRange + ";" + maxValueInRange + "\n")
            } catch {
             case e => {} //println(min);println(max)
            }
           }
           case SingleValueRange(v) => {single += 1}
          }
          if (minMax > 0 && single > 0)
           throw new Exception("Both MinMax and Single in one expression")
          if (minMax > 1)
           throw new Exception("Two MinMaxs defined")
         })
        }
        case None => {}
       }
      })
      
//      out.write(f.getName + ";" + intNumber + ";" + stringNumber + ";"  + mixedNumber + "\n")

      out.flush
  }
}
