/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package gsd.cdl.parser.adapter

import org.junit._
import Assert._

import org.kiama.rewriting.Rewriter._

import org.junit._
import org.junit.Assert._
import org.scalatest.junit._
import gsd.cdl.model._
import gsd.iml.parser._
import gsd.iml.ast.feature._
import gsd.iml.ast.constraint._
import gsd.iml.ast.expression._
import gsd.cdl.test.util.TestFile

class Enums extends JUnitSuite {

  case class EnumInfo(fileName:String, id:String, size:Int) {
    override def toString = {
      fileName + "\t" + id + "\t" + size
    }
  }

      @Test
    def testMock() = {
      assertTrue(true)
    }

//    @Test
    def testAllImlFiles() = {
        var totalCount = 0;
        var enumsList = scala.collection.mutable.ListBuffer[EnumInfo]()

        for (file <- new java.io.File(TestFile.getRelativeToProjectRoot("/input/iml/")).listFiles(new java.io.FilenameFilter() {
            def accept(dir:java.io.File, name:String):Boolean = {
                return name.endsWith(".iml") ;
            }
        })) {
        println("Processing file: " + file)

        var nodesById = scala.collection.mutable.Map[String, Node]()
        var currentCount = 0;

        var topLevelNodes = scala.collection.mutable.ListBuffer[Node]()
        val imlResult = ImlParser.parse(file)
        if (!gsd.iml.util.CollectionsUtils.isEmpty(imlResult)) {
          val iterator = imlResult.iterator
          while(iterator.hasNext) {
            val aNode = iterator.next
            val translatedNode = ImlFeatureToNode(aNode)
            topLevelNodes += (translatedNode)
          }
        }

        collectl {
          case n:Node => {
            nodesById += (n.id -> n)
          }
        } (topLevelNodes.toList)

        nodesById.foreach(keyvalue => {
          if (isEnumeration(keyvalue._2)) {
//            println("Option id: " + keyvalue._1)
//            println("Size: " + keyvalue._2.legalValues.getOrElse(null).ranges.size + ", [" + getRepresentation(keyvalue._2.legalValues.getOrElse(null).ranges) + "]")
            totalCount = totalCount + 1;
            currentCount = currentCount + 1;
            val num = keyvalue._2.legalValues.getOrElse(null).ranges.size
            enumsList += EnumInfo(file.toString, keyvalue._1.toString, num)
          }
        })

//        println("Enums in this file: " + currentCount)
        currentCount = 0
       }
       println("Total enums in all files: " + totalCount)
    val out = new java.io.FileWriter("result.csv")
    enumsList.toList.foreach(enum => out.write(enum.toString + "\n"))
    out.close
    }


  private def getRepresentation(ranges:List[Range]):String = {
    var buffer = new StringBuilder()
    ranges.foreach (range => {
      range match {
        case SingleValueRange(v) => {
            if (v.isInstanceOf[StringLiteral]) {
              buffer.append(v.asInstanceOf[StringLiteral].value).append(" ")
            } else if (v.isInstanceOf[LongIntLiteral]) {
              buffer.append(v.asInstanceOf[LongIntLiteral].value).append(" ")
            }
        }
        case _ => "WRONG"
      }
    })
    buffer.toString
  }

  def isEnumeration(node: Node): Boolean = {
    if (node.legalValues != None) {
      val Some(LegalValuesOption(ranges)) = node.legalValues
      (ranges.collect{case range: SingleValueRange => range}.
       filter(isCandidateForEnum).size > 0) // we have at least one string value
    } else
      false
  }

  /**
   * A SingleValueRange is a candidate for enum if
   * it is not an integer vlaue, i.e. Only Strings are enums
   */
  def isCandidateForEnum(expr:SingleValueRange) : Boolean = {
    expr.v match {
      case StringLiteral(v) =>
              val numberPattern = """0[xX]([0123456789abcdefABCDEF]+)""".r
              numberPattern.unapplySeq(v) match {
                      case Some(List(numberPart)) => false
                      case None => {
                            true
                       }
            }
      case _ => false
    }
  }


}
