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
import gsd.iml.ast.feature._
import gsd.iml.ast.constraint._
import gsd.iml.ast.expression._
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
      if (!testFeatureToNodeId(node, feature)) { //id
        false
      } else if (!testFeatureToNodeDisplay(node, feature)) { //display
        false
      } else if (!testFeatureToNodeType(node, feature)) { // cdlType
        false
      } else if (!testFeatureToNodeDescription(node, feature)) { // description
        false
      } else if (!testFeatureToNodeFlavor(node, feature)) { // flavor
        false
      } else if (!testCDLExpressionToConstraint(node.defaultValue, feature.getDefaultValue)) { // flavor
        false
      } else {
        true
      }
    }

  private def testCDLExpressionToConstraint(cdlExpression:Option[CDLExpression], constraint:ImlConstraint):Boolean = {
    if (cdlExpression == None && constraint == null) {
      true
    } else {
      if (constraint.isInstanceOf[DefaultValueConstraint]) {
        testCDLExpressionToParsedExpression(cdlExpression, constraint.asInstanceOf[DefaultValueConstraint].getExpression)
      } else {
        false
      }
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

  private def testCDLExpressionToParsedExpression(cdlExpression:CDLExpression, expression:Expression):Boolean = {
    testCDLExpressionToParsedExpression(Some[CDLExpression](cdlExpression), expression)
  }

  private def testCDLExpressionToParsedExpression(cdlExpression:Option[CDLExpression], expression:Expression):Boolean = {
    if (cdlExpression == None && expression == null) {
      true
    } else if (cdlExpression != None && expression != null) {
        cdlExpression.getOrElse(null) match {
        case StringLiteral(v) => {expression.isInstanceOf[StringLiteralExpression] && expression.asInstanceOf[StringLiteralExpression].get() == v}
        case LongIntLiteral(v) => {expression.isInstanceOf[LongLiteralExpression] && expression.asInstanceOf[LongLiteralExpression].get() == v}
        case Identifier(v) => {expression.isInstanceOf[IdentifierExpression] && expression.asInstanceOf[IdentifierExpression].getId == v}
        case Conditional(cond, pass, fail) => {
                  expression.isInstanceOf[ConditionalExpression] &&
                  testCDLExpressionToParsedExpression(cond, expression.asInstanceOf[ConditionalExpression].getCondition) &&
                  testCDLExpressionToParsedExpression(pass, expression.asInstanceOf[ConditionalExpression].getPass) &&
                  testCDLExpressionToParsedExpression(fail, expression.asInstanceOf[ConditionalExpression].getFail)
        }
        case Or(left, right) => {
              expression.isInstanceOf[OrExpression] &&
              testCDLExpressionToParsedExpression(left, expression.asInstanceOf[OrExpression].getLeft) &&
              testCDLExpressionToParsedExpression(right, expression.asInstanceOf[OrExpression].getRight)
            }
        case And(left, right) => {
              expression.isInstanceOf[AndExpression] &&
              testCDLExpressionToParsedExpression(left, expression.asInstanceOf[AndExpression].getLeft) &&
              testCDLExpressionToParsedExpression(right, expression.asInstanceOf[AndExpression].getRight)
          }
        case Eq(left, right) => {
              expression.isInstanceOf[EqualExpression] &&
              testCDLExpressionToParsedExpression(left, expression.asInstanceOf[EqualExpression].getLeft) &&
              testCDLExpressionToParsedExpression(right, expression.asInstanceOf[EqualExpression].getRight)
          }
        case NEq(left, right) => {
            expression.isInstanceOf[NotEqualExpression] &&
            testCDLExpressionToParsedExpression(left, expression.asInstanceOf[NotEqualExpression].getLeft) &&
            testCDLExpressionToParsedExpression(right, expression.asInstanceOf[NotEqualExpression].getRight)
          }
        case LessThan(left, right) => {
            expression.isInstanceOf[LessThanExpression] &&
            testCDLExpressionToParsedExpression(left, expression.asInstanceOf[LessThanExpression].getLeft) &&
            testCDLExpressionToParsedExpression(right, expression.asInstanceOf[LessThanExpression].getRight)
          }
        case LessThanOrEq(left, right) => {
            expression.isInstanceOf[LessThanEqualExpression] &&
            testCDLExpressionToParsedExpression(left, expression.asInstanceOf[LessThanEqualExpression].getLeft) &&
            testCDLExpressionToParsedExpression(right, expression.asInstanceOf[LessThanEqualExpression].getRight)
          }
        case GreaterThan(left, right) => {
            expression.isInstanceOf[GreaterThanExpression] &&
            testCDLExpressionToParsedExpression(left, expression.asInstanceOf[GreaterThanExpression].getLeft) &&
            testCDLExpressionToParsedExpression(right, expression.asInstanceOf[GreaterThanExpression].getRight)
          }
        case GreaterThanOrEq(left, right) => {
            expression.isInstanceOf[GreaterThanEqualExpression] &&
            testCDLExpressionToParsedExpression(left, expression.asInstanceOf[GreaterThanEqualExpression].getLeft) &&
            testCDLExpressionToParsedExpression(right, expression.asInstanceOf[GreaterThanEqualExpression].getRight)
          }
        case Plus(left,right) => {
            expression.isInstanceOf[PlusExpression] &&
            testCDLExpressionToParsedExpression(left, expression.asInstanceOf[PlusExpression].getLeft) &&
            testCDLExpressionToParsedExpression(right, expression.asInstanceOf[PlusExpression].getRight)
          }
        case Minus(left,right) => {
            expression.isInstanceOf[MinusExpression] &&
            testCDLExpressionToParsedExpression(left, expression.asInstanceOf[MinusExpression].getLeft) &&
            testCDLExpressionToParsedExpression(right, expression.asInstanceOf[MinusExpression].getRight)
          }
        case Dot(left,right) => {
            expression.isInstanceOf[DotExpression] &&
            testCDLExpressionToParsedExpression(left, expression.asInstanceOf[DotExpression].getLeft) &&
            testCDLExpressionToParsedExpression(right, expression.asInstanceOf[DotExpression].getRight)
          }
        case Times(left,right) => {
            expression.isInstanceOf[TimesExpression] &&
            testCDLExpressionToParsedExpression(left, expression.asInstanceOf[TimesExpression].getLeft) &&
            testCDLExpressionToParsedExpression(right, expression.asInstanceOf[TimesExpression].getRight)
          }
        case Div(left,right) => {
            expression.isInstanceOf[DivideExpression] &&
            testCDLExpressionToParsedExpression(left, expression.asInstanceOf[DivideExpression].getLeft) &&
            testCDLExpressionToParsedExpression(right, expression.asInstanceOf[DivideExpression].getRight)
          }
        case Mod(left,right) => {
            expression.isInstanceOf[ModExpression] &&
            testCDLExpressionToParsedExpression(left, expression.asInstanceOf[ModExpression].getLeft) &&
            testCDLExpressionToParsedExpression(right, expression.asInstanceOf[ModExpression].getRight)
          }
        case Not(e) => {
            expression.isInstanceOf[NotExpression] &&
            testCDLExpressionToParsedExpression(e, expression.asInstanceOf[NotExpression].getExpression)
          }
        case FunctionCall("is_substr", scala.List(whole, sub)) => {true}
        case FunctionCall("bool", scala.List(e)) => {true}
        case True() => {true}
        case False() => {true}
        case _ => throw new Exception("Unexpected expression: " + cdlExpression)

        }
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

  private def testFeatureToNodeFlavor(node:Node, feature:gsd.iml.ast.feature.Feature):Boolean = {
    node.flavor match {
      case NoneFlavor => feature.getFlavor.isInstanceOf[gsd.iml.ast.flavor.NoneFlavor]
      case BoolFlavor => feature.getFlavor.isInstanceOf[gsd.iml.ast.flavor.BoolFlavor]
      case DataFlavor => feature.getFlavor.isInstanceOf[gsd.iml.ast.flavor.DataFlavor]
      case BoolDataFlavor => feature.getFlavor.isInstanceOf[gsd.iml.ast.flavor.BoolDataFlavor]
      case _ => throw new Exception("Unknown CDL flavor: " + node.flavor.getClass)
    }
  }

  private def testFeatureToNodeDescription(node:Node, feature:gsd.iml.ast.feature.Feature):Boolean = {
      if ((node.description != None && feature.getDescription() != null)) {
        // the value is the same
        node.description.getOrElse("") == feature.getDescription()
      } else if (node.description == None && feature.getDescription() == null) {
        true
      } else {
        false
      }
  }

  private def testFeatureToNodeType(node:Node, feature:gsd.iml.ast.feature.Feature):Boolean = {
    node.cdlType match {
      case InterfaceType => feature.getType.isInstanceOf[InterfaceFeatureType]
      case PackageType => feature.getType.isInstanceOf[PackageFeatureType]
      case ComponentType => feature.getType.isInstanceOf[ComponentFeatureType]
      case OptionType => feature.getType.isInstanceOf[OptionFeatureType]
      case _ => throw new Exception("Unknown CDL type: " + node.cdlType.getClass)
    }
  }

}
