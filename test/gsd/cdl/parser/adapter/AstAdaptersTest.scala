/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package gsd.cdl.parser.adapter

import org.junit._
import org.junit.Assert._
import org.scalatest.junit._
import gsd.cdl.model._
import gsd.iml.parser._
import gsd.iml.ast.feature.Feature
import java.util.List;
import gsd.cdl.test.util.TestFile

class AstAdaptersTest extends JUnitSuite {

  var originalFeatures = scala.collection.immutable.List[gsd.iml.ast.feature.Feature]()
  var convertedNodes = scala.collection.immutable.List[Node]()

  @Before
    def setUp: Unit = {
      var features = scala.collection.mutable.ListBuffer[gsd.iml.ast.feature.Feature]()
      var nodes = scala.collection.mutable.ListBuffer[Node]()
      val imlResult = ImlParser.parse(TestFile.get("gsd/cdl/parser/adapter/sample.iml"))
      if (!gsd.iml.util.CollectionsUtils.isEmpty(imlResult)) {
        val iterator = imlResult.iterator
        while(iterator.hasNext) {
          val aNode = iterator.next
          nodes += (ImlFeatureToNode(aNode))
          features += (aNode)
        }
      }

      originalFeatures = features.toList
      convertedNodes = nodes.toList
    }

    @Test
    def translateTest() = {
     assertEquals(originalFeatures.size, convertedNodes.size)
     for (i <- 0 to originalFeatures.size - 1) {
      assertTrue(testFeatureToNode(convertedNodes.apply(i), originalFeatures.apply(i)))
     }
    }
    
    private def testFeatureToNode(node:Node, feature:gsd.iml.ast.feature.Feature):Boolean = {
      if (!testFeatureToNodeId(node, feature)) {
        false
      } else if (!testFeatureToNodeDisplay(node, feature)) {
        false
      } else if (!testFeatureToNodeType(node, feature)) {
        false
      } else {
        true
      }
    }

  private def testFeatureToNodeId(node:Node, feature:gsd.iml.ast.feature.Feature):Boolean = {
      if ((node.id != null && feature.getId() != null)) {
        // the value is the same
        node.id == feature.getId()
      } else if ((node.id == null && feature.getId() == null)) {
        true
      } else {
        false
      }
  }

  private def testFeatureToNodeDisplay(node:Node, feature:gsd.iml.ast.feature.Feature):Boolean = {
      if ((node.display != null && feature.getDisplay() != null)) {
        // the value is the same
        node.display == feature.getDisplay()
      } else if ((node.display == null && feature.getDisplay() == null)) {
        true
      } else {
        false
      }
  }

  private def testFeatureToNodeType(node:Node, feature:gsd.iml.ast.feature.Feature):Boolean = {
    //node.cdlType ==
    // mock
    true
  }

}
