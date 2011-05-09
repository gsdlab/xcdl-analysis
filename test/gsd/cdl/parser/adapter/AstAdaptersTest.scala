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
      val imlResult = ImlParser.parse(TestFile.get("gsd/cdl/parser/adapter/adderII.iml"))
      if (!gsd.iml.util.CollectionsUtils.isEmpty(imlResult)) {
        val iterator = imlResult.iterator
        while(iterator.hasNext) {
          val aNode = iterator.next
          val translatedNode = ImlFeatureToNode(aNode)
          nodes += (translatedNode)
          features += (aNode)
        }
      }

      originalFeatures = features.toList
      convertedNodes = nodes.toList
    }

    @Test
    def testAll() = {
        for (file <- new java.io.File(TestFile.get("gsd/cdl/parser/adapter/iml/")).listFiles(new java.io.FilenameFilter() {
            def accept(dir:java.io.File, name:String):Boolean = {
                return name.endsWith(".iml") ;
            }
        })) {
          println("Testing file: " + file)

          var features = scala.collection.mutable.ListBuffer[gsd.iml.ast.feature.Feature]()
          var nodes = scala.collection.mutable.ListBuffer[Node]()
          val imlResult = ImlParser.parse(file)
          if (!gsd.iml.util.CollectionsUtils.isEmpty(imlResult)) {
            val iterator = imlResult.iterator
            while(iterator.hasNext) {
              val aNode = iterator.next
              val translatedNode = ImlFeatureToNode(aNode)
              nodes += (translatedNode)
              features += (aNode)
            }
          }

          originalFeatures = features.toList
          convertedNodes = nodes.toList
          assertEquals(originalFeatures.size, convertedNodes.size)
          for (i <- 0 to originalFeatures.size - 1) {
            assertTrue(testFeatureToNode(convertedNodes.apply(i), originalFeatures.apply(i)))
          }
        }
    }

//    @Test
    def translateTest() = {
     assertEquals(originalFeatures.size, convertedNodes.size)
     for (i <- 0 to originalFeatures.size - 1) {
      assertTrue(testFeatureToNode(convertedNodes.apply(i), originalFeatures.apply(i)))
     }
    }
    
    private def testFeatureToNode(node:Node, feature:gsd.iml.ast.feature.Feature):Boolean = {
//      println("Comparing: " + node.id + ", and: " + feature.getId)
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
      } else if (!testCDLExpressionToConstraint(node.defaultValue, feature.getDefaultValue)) { // default value
        println("Failed default")
        false
      } else if (!testCDLExpressionToConstraint(node.calculated, feature.getCalculated)) { // calculated
        println("Failed calculated")
        false
      } else if (!testLegalValuesOptionsToLegalValuesConstraint(node.legalValues, feature.getLegalValues)) { // legal values
        println("Failed legal")
        false
      } else if (!testRequires(node.reqs, feature.getRequires)) { // requires
        println("Failed reqs")
        false
      } else if (!testActiveIfs(node.activeIfs, feature.getActiveIfs)) { // active ifs
        println("Failed active ifs")
        false
      } else if (!testImplements(node.implements, feature.getImpls)) { // implements
        println("Failed implements")
        false
      } else if (!testChildNodes(node.children, feature.getSubfeatures)) { // implements
        false
      } else {
        true
      }
    }

  private def testChildNodes(nodes: scala.List[Node], children:java.util.List[Feature]):Boolean = {
    if (nodes.size != 0 && children != null) {
      if (nodes.size == children.size) {
        var pass = true
        for (i <- 0 to nodes.size - 1) {
          pass = pass && testFeatureToNode(nodes.apply(i), children.get(i))
        }
        pass
      } else {
        false
      }
    } else if (nodes.size == 0 && children == null) {
      true
    } else {
      false
    }
  }

  private def testLegalValuesOptionsToLegalValuesConstraint(legalValues:Option[LegalValuesOption], constraint:LegalValuesConstraint):Boolean = {
    if (legalValues == None && constraint == null) {
      true
    } else {
      if (legalValues == None && constraint != null && constraint.getExpressions().size == 0) {
        true
      } else {
        if (legalValues != None) {
          var pass = true
          val values = legalValues.getOrElse(null)
          for (i <- 0 to values.ranges.size - 1) {
            if (legalValues.getOrElse(null).ranges.apply(i).isInstanceOf[MinMaxRange]) {
                // println(constraint.getExpressions().get(i).getClass)
                if (constraint.getExpressions().get(i).isInstanceOf[IntervalExpression]) {
                  pass = pass && 
                  testCDLExpressionToParsedExpression(
                    legalValues.getOrElse(null).ranges.apply(i).asInstanceOf[MinMaxRange].low,
                                                                     constraint.getExpressions().get(i).asInstanceOf[IntervalExpression].getFrom)

                  pass = pass &&  testCDLExpressionToParsedExpression(legalValues.getOrElse(null).ranges.apply(i).asInstanceOf[MinMaxRange].high,
                                                                     constraint.getExpressions().get(i).asInstanceOf[IntervalExpression].getTo)
              } else {
                  false
                }
            } else {
                pass = pass &&
                testCDLExpressionToParsedExpression(values.ranges.apply(i).asInstanceOf[SingleValueRange].v,
                                                            constraint.getExpressions().get(i))
            }
          }
          pass
        } else {
          false
        }
      }
    }
  }

  private def convertToList[I <: O, O](input:java.util.List[I]): scala.List[O] = {
    var list = scala.collection.mutable.ListBuffer[O]()
    if (!gsd.iml.util.CollectionsUtils.isEmpty(input)) {
      val iterator = input.iterator
      while(iterator.hasNext) {
        list += iterator.next
      }
      if (list.toList.size != input.size)
        throw new Exception("Wrong conversion")
    }
    list.toList
  }

  private def testRequires(cdlExpressions:scala.List[CDLExpression], requiresConstraints:java.util.List[RequiresConstraint]):Boolean = {
    testListOfCDLExpressionsToFeatureConstraint(cdlExpressions, convertToList[RequiresConstraint, UnaryImlConstraint](requiresConstraints))
  }

  private def testActiveIfs(cdlExpressions:scala.List[CDLExpression], activeIfs:java.util.List[ActiveIfConstraint]):Boolean = {
    testListOfCDLExpressionsToFeatureConstraint(cdlExpressions, convertToList[ActiveIfConstraint, UnaryImlConstraint](activeIfs))
  }

  private def testImplements(cdlExpressions:scala.List[CDLExpression], implements:java.util.List[ImplementsConstraint]):Boolean = {
    testListOfCDLExpressionsToFeatureConstraint(cdlExpressions, convertToList[ImplementsConstraint, UnaryImlConstraint](implements))
  }

  private def testListOfCDLExpressionsToFeatureConstraint(cdlExpressions:scala.List[CDLExpression], constraints:scala.List[UnaryImlConstraint]):Boolean = {
    if (cdlExpressions != null && constraints != null) {
      if (cdlExpressions.size == constraints.size) {
          var pass = true
          for (i <- 0 to cdlExpressions.size - 1) {
            pass = pass && testCDLExpressionToConstraint(cdlExpressions.apply(i), constraints.apply(i))
          }
          pass
      } else {
//        println("Size doesn't match. Size of first argument: " + cdlExpressions.size + ", and of the second: " + constraints.size)
        false
      }
    } else if (cdlExpressions == null && constraints == null) {
      true
    } else {
      false
    }
  }

  private def testCDLExpressionToConstraint(cdlExpression:CDLExpression, constraint:ImlConstraint):Boolean = {
    testCDLExpressionToConstraint(Some[CDLExpression](cdlExpression), constraint)
  }

  private def testCDLExpressionToConstraint(cdlExpression:Option[CDLExpression], constraint:ImlConstraint):Boolean = {
    if (cdlExpression == None && constraint == null) {
      true
    } else {
      if (constraint.isInstanceOf[DefaultValueConstraint]) {
        testCDLExpressionToParsedExpression(cdlExpression, constraint.asInstanceOf[DefaultValueConstraint].getExpression)
      } else if (constraint.isInstanceOf[CalculatedConstraint]) {
        testCDLExpressionToParsedExpression(cdlExpression, constraint.asInstanceOf[CalculatedConstraint].getExpression)
      } else if (constraint.isInstanceOf[RequiresConstraint]) {
        testCDLExpressionToParsedExpression(cdlExpression, constraint.asInstanceOf[RequiresConstraint].getExpression)
      } else if (constraint.isInstanceOf[ActiveIfConstraint]) {
        testCDLExpressionToParsedExpression(cdlExpression, constraint.asInstanceOf[ActiveIfConstraint].getExpression)
      } else if (constraint.isInstanceOf[ImplementsConstraint]) {
        testCDLExpressionToParsedExpression(cdlExpression, constraint.asInstanceOf[ImplementsConstraint].getExpression)
      } else {
        throw new Exception ("Wrong constraint class: " + constraint.getClass)
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
        case FunctionCall("is_substr", scala.List(whole, sub)) => {
            testCDLExpressionToParsedExpression(whole, expression.asInstanceOf[IsSubstringFunctionCallExpression].getFirstArgument) &&
            testCDLExpressionToParsedExpression(sub, expression.asInstanceOf[IsSubstringFunctionCallExpression].getSecondArgument)
          }
        case FunctionCall("is_loaded", scala.List(exp)) => {
            testCDLExpressionToParsedExpression(exp, expression.asInstanceOf[IsLoadedFunctionCallExpression].getArgument)
          }
        case FunctionCall("is_active", scala.List(exp)) => {
            expression.isInstanceOf[IsActiveFunctionCallExpression] &&
            testCDLExpressionToParsedExpression(exp, expression.asInstanceOf[IsActiveFunctionCallExpression].getArgument)
          }
//        case FunctionCall("bool", scala.List(e)) => {true}
        case True() => {
            expression.asInstanceOf[BooleanLiteralExpression].get == java.lang.Boolean.TRUE
          }
        case False() => {
            expression.asInstanceOf[BooleanLiteralExpression].get == java.lang.Boolean.FALSE
          }
        case Implies(left, right) => {
              expression.isInstanceOf[ImpliesExpression] &&
              testCDLExpressionToParsedExpression(left, expression.asInstanceOf[ImpliesExpression].getLeft) &&
              testCDLExpressionToParsedExpression(right, expression.asInstanceOf[ImpliesExpression].getRight)
            }
        case BtOr(left, right) => {
              expression.isInstanceOf[BitwiseOrExpression] &&
              testCDLExpressionToParsedExpression(left, expression.asInstanceOf[BitwiseOrExpression].getLeft) &&
              testCDLExpressionToParsedExpression(right, expression.asInstanceOf[BitwiseOrExpression].getRight)
            }
        case BtXor(left, right) => {
              expression.isInstanceOf[BitwiseXorExpression] &&
              testCDLExpressionToParsedExpression(left, expression.asInstanceOf[BitwiseXorExpression].getLeft) &&
              testCDLExpressionToParsedExpression(right, expression.asInstanceOf[BitwiseXorExpression].getRight)
            }
        case BtAnd(left, right) => {
              expression.isInstanceOf[BitwiseAndExpression] &&
              testCDLExpressionToParsedExpression(left, expression.asInstanceOf[BitwiseAndExpression].getLeft) &&
              testCDLExpressionToParsedExpression(right, expression.asInstanceOf[BitwiseAndExpression].getRight)
            }

        case _ => throw new Exception("Unsupported CDLExpression class for conversion: " + cdlExpression
                                      + " with class" + cdlExpression.getClass)
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
