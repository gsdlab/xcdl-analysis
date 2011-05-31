package gsd.cdl.formula

import org.junit._
import org.junit.Assert._
import org.scalatest.junit._
import gsd.cdl.model._
import gsd.iml.parser._
import gsd.iml.ast.feature._
import gsd.iml.ast.constraint._
import gsd.iml.ast.expression._
import java.util.List;
import gsd.cdl.test.util.TestFile
import gsd.cdl.parser.EcosIML
import gsd.cdl.formula.Enumerations._

class Enumerationstest extends JUnitSuite {

  // test files should have only one option that will be enumeration (or not)

   @Test
   def test1() = {
     val nodes = EcosIML.parseFile(TestFile.get("gsd/cdl/formula/enumTest1.iml"))
     for (i <- 0 to nodes.size - 1)
      assertTrue(isEnumeration(nodes.apply(i)))
   }

   @Test
   def test2() = {
     val nodes = EcosIML.parseFile(TestFile.get("gsd/cdl/formula/enumTest2.iml"));
     for (i <- 0 to nodes.size - 1)
      assertFalse(isEnumeration(nodes.apply(i)))
   }
}
