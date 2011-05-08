/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package gsd.cdl.parser.adapter

import org.junit._
import org.junit.Assert._
import org.scalatest.junit._
import gsd.cdl.model._
import gsd.iml.parser._;
import gsd.iml.ast.feature.Feature;
import java.util.List;

class AstAdaptersTest extends JUnitSuite {

  protected def SEP = System.getProperty("file.separator")
  protected def DIR = System.getProperty("user.dir") + SEP + "test" + SEP + "gsd" + SEP + "cdl" + SEP + "parser" + SEP + "adapter" + SEP

  var features = scala.List[gsd.iml.ast.feature.Feature]()
  var convertedNodes = scala.List[Node]()

  @Before
    def setUp: Unit = {
      val imlResult = ImlParser.parse(DIR + "sample.iml")
      println("Size of parsed: " + imlResult.size)
      if (!gsd.iml.util.CollectionsUtils.isEmpty(imlResult)) {
        val iterator = imlResult.iterator
        while(iterator.hasNext) {
          val aNode = iterator.next
          convertedNodes.+:(ImlFeatureToNode(aNode))
          features.+:(aNode)
        }
      }

      println(features.size)
      println(convertedNodes.size)
    }

    @After
    def tearDown: Unit = {
    }

    @Test
    def translateSample0Test() = {
     assertEquals(features.size, convertedNodes.size)
    }
    
    private def testFeatureToNode(node:Node, feature:gsd.iml.ast.feature.Feature):Boolean = {
      true
    }

}
