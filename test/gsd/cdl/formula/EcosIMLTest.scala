/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package gsd.cdl.formula

import org.junit._
import Assert._

import gsd.cdl.EcosIML
import gsd.cdl.test.util.TestFile

class EcosIMLTest {

    @Test
    def testShowCase = {
      val nodes = EcosIML.parseFile(TestFile.get("gsd/cdl/formula/showcase"))
      println(nodes)
    }
}
