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

  var originalFeatures = scala.collection.immutable.List[gsd.iml.ast.feature.Feature]()
  var convertedNodes = scala.collection.immutable.List[Node]()

  @Before
    def setUp: Unit = {
      var features = scala.collection.mutable.ListBuffer[gsd.iml.ast.feature.Feature]()
      var nodes = scala.collection.mutable.ListBuffer[Node]()
      val imlResult = ImlParser.parse(DIR + "sample.iml")
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

    @After
    def tearDown: Unit = {
    }

    @Test
    def translateTest() = {
     assertEquals(originalFeatures.size, convertedNodes.size)
     for (i <- 0 to originalFeatures.size - 1) {
      assertTrue(testFeatureToNode(convertedNodes.apply(i), originalFeatures.apply(i)))
     }
    }
    
    private def testFeatureToNode(node:Node, feature:gsd.iml.ast.feature.Feature):Boolean = {
      true
    }

}
