/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package gsd.cdl.model

import gsd.iml.ast.expression._
import gsd.cdl.formula._
import gsd.iml.ast.feature._
import gsd.iml.ast.constraint._

object ImlExpressionToCDLExpression {

  def apply(e:Expression) : CDLExpression = {

    if (e.isInstanceOf[AndExpression]) {
      val exp = e.asInstanceOf[AndExpression]
      return new And(ImlExpressionToCDLExpression(exp.getLeft()),
                      ImlExpressionToCDLExpression(exp.getRight()))
    }

    if (e.isInstanceOf[OrExpression]) {
      val exp = e.asInstanceOf[OrExpression]
      return new Or(ImlExpressionToCDLExpression(exp.getLeft()),
                     ImlExpressionToCDLExpression(exp.getRight()))
    }

    if (e.isInstanceOf[BitwiseAndExpression]) {
      val exp = e.asInstanceOf[BitwiseAndExpression]
      return new BtAnd(ImlExpressionToCDLExpression(exp.getLeft()),
                        ImlExpressionToCDLExpression(exp.getRight()))
    }

    if (e.isInstanceOf[BitwiseOrExpression]) {
      val exp = e.asInstanceOf[BitwiseOrExpression]
      return new BtOr(ImlExpressionToCDLExpression(exp.getLeft()),
                      ImlExpressionToCDLExpression(exp.getRight()))
    }

    if (e.isInstanceOf[BitwiseXorExpression]) {
      val exp = e.asInstanceOf[BitwiseXorExpression]
      return new BtXor(ImlExpressionToCDLExpression(exp.getLeft()),
                        ImlExpressionToCDLExpression(exp.getRight()))
    }

    if (e.isInstanceOf[BitwiseLeftShiftExpression]) {
      val exp = e.asInstanceOf[BitwiseLeftShiftExpression]
      return new BtLeft(ImlExpressionToCDLExpression(exp.getLeft()),
                        ImlExpressionToCDLExpression(exp.getRight()))
    }

    if (e.isInstanceOf[BitwiseRightShiftExpression]) {
      val exp = e.asInstanceOf[BitwiseRightShiftExpression]
      return new BtRight(ImlExpressionToCDLExpression(exp.getLeft()),
                          ImlExpressionToCDLExpression(exp.getRight()))
    }

    if (e.isInstanceOf[FunctionCallExpression]) {
      val exp = e.asInstanceOf[FunctionCallExpression]
      val args = List[CDLExpression]()

      val iterator = exp.getArguments().iterator ;
      while(iterator.hasNext) {
        args.+:(iterator.next)
      }

      return new FunctionCall(exp.getName(), args)
   }

    if (e.isInstanceOf[EqualExpression]) {
      val exp = e.asInstanceOf[EqualExpression]
      return new Eq(ImlExpressionToCDLExpression(exp.getLeft()),
                    ImlExpressionToCDLExpression(exp.getRight))
    }

    if (e.isInstanceOf[NotEqualExpression]) {
      val exp = e.asInstanceOf[NotEqualExpression]
      return new NEq(ImlExpressionToCDLExpression(exp.getLeft()),
                     ImlExpressionToCDLExpression(exp.getRight))
    }

    if (e.isInstanceOf[LessThanExpression]) {
      val exp = e.asInstanceOf[LessThanExpression]
      return new LessThan(ImlExpressionToCDLExpression(exp.getLeft()),
                          ImlExpressionToCDLExpression(exp.getRight))
    }

    if (e.isInstanceOf[LessThanEqualExpression]) {
      val exp = e.asInstanceOf[LessThanEqualExpression]
      return new LessThanOrEq(ImlExpressionToCDLExpression(exp.getLeft()),
                              ImlExpressionToCDLExpression(exp.getRight))
    }

    if (e.isInstanceOf[GreaterThanExpression]) {
      val exp = e.asInstanceOf[GreaterThanExpression]
      return new GreaterThan(ImlExpressionToCDLExpression(exp.getLeft()),
                             ImlExpressionToCDLExpression(exp.getRight))
    }

    if (e.isInstanceOf[GreaterThanEqualExpression]) {
      val exp = e.asInstanceOf[GreaterThanEqualExpression]
      return new GreaterThanOrEq(ImlExpressionToCDLExpression(exp.getLeft()),
                                 ImlExpressionToCDLExpression(exp.getRight))
    }

    if (e.isInstanceOf[ModExpression]) {
      val exp = e.asInstanceOf[ModExpression]
      return new Mod(ImlExpressionToCDLExpression(exp.getLeft()),
                      ImlExpressionToCDLExpression(exp.getRight))
    }

    if (e.isInstanceOf[PlusExpression]) {
      val exp = e.asInstanceOf[PlusExpression]
      return new Plus(ImlExpressionToCDLExpression(exp.getLeft()),
                      ImlExpressionToCDLExpression(exp.getRight))
    }

    if (e.isInstanceOf[MinusExpression]) {
      val exp = e.asInstanceOf[MinusExpression]
      return new Minus(ImlExpressionToCDLExpression(exp.getLeft()),
                        ImlExpressionToCDLExpression(exp.getRight))
    }

    if (e.isInstanceOf[TimesExpression]) {
      val exp = e.asInstanceOf[TimesExpression]
      return new Times(ImlExpressionToCDLExpression(exp.getLeft()),
                       ImlExpressionToCDLExpression(exp.getRight))
    }

    if (e.isInstanceOf[DivideExpression]) {
      val exp = e.asInstanceOf[DivideExpression]
      return new Div(ImlExpressionToCDLExpression(exp.getLeft()),
                      ImlExpressionToCDLExpression(exp.getRight))
    }

    if (e.isInstanceOf[DotExpression]) {
      val exp = e.asInstanceOf[DotExpression]
      return new Dot(ImlExpressionToCDLExpression(exp.getLeft()),
                     ImlExpressionToCDLExpression(exp.getRight))
    }

    if (e.isInstanceOf[NotExpression]) {
      val exp = e.asInstanceOf[NotExpression]
      return new Not(ImlExpressionToCDLExpression(exp.getExpression()))
    }

    if (e.isInstanceOf[IdentifierExpression]) {
      val exp = e.asInstanceOf[IdentifierExpression]
      return new Identifier(exp.getId)
    }

    if (e.isInstanceOf[LongLiteralExpression]) {
      val exp = e.asInstanceOf[LongLiteralExpression]
      return new LongIntLiteral(exp.get().longValue())
    }

    if (e.isInstanceOf[StringLiteralExpression]) {
      val exp = e.asInstanceOf[StringLiteralExpression]
      return new StringLiteral(exp.get())
    }

    throw new Exception("Unknown adapter for " + e.getClass().getName())
  }
}

/* . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

object ImlFeatureToNode {
  def apply(f:Feature) : Node = {
    val id = f.getId

    val cdlType = ImlFeatureTypeToCDLType(f.getType())

    val display = f.getDisplay()

    var description : Option[String] = null
    if (f.getDescription == null)
      description = Some(null)
    else
      description = Some(f.getDescription)

    val flavor = ImlFlavorToCDLFlavor(f.getFlavor())

    var defaultValue : Option[CDLExpression] = null
    if (f.getDefaultValue() == null)
      defaultValue = Some(null)
    else
      defaultValue = Some(ImlExpressionToCDLExpression(f.getDefaultValue().getExpression()))

    var calculated : Option[CDLExpression] = null
    if (f.getCalculated() == null)
      calculated = Some(null)
    else
     calculated = Some(ImlExpressionToCDLExpression(f.getCalculated().getExpression()))

    var legalValues : Option[LegalValuesOption] = null
    if (f.getLegalValues() == null)
      legalValues = Some(null)
    else
      legalValues = Some(ImlLegalValuesConstraintToCDLValuesOption(f.getLegalValues()))

    var reqs = ImlConstraintsToCDLExpressions(f.getRequires)
    var activeIf = ImlConstraintsToCDLExpressions(f.getActiveIfs)
    var impls = ImlConstraintsToCDLExpressions(f.getImpls)
    var children = ImlFeatureListToImlNodeList(f.getSubfeatures)

    return new Node(
       id,
       cdlType,
       display,
       description,
       flavor,
       defaultValue,
       calculated,
       legalValues,
       reqs,
       activeIf,
       impls,
       children)
  }
}

/* . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

object ImlFeatureTypeToCDLType {
  def apply(t:FeatureType) : CDLType = {
    if (t.isInstanceOf[ComponentFeatureType])
      return ComponentType

    if (t.isInstanceOf[InterfaceFeatureType])
      return InterfaceType

    if (t.isInstanceOf[PackageFeatureType])
      return PackageType

    return OptionType
  }
}

/* . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

object ImlFlavorToCDLFlavor {
  def apply(f:gsd.iml.ast.flavor.Flavor) : gsd.cdl.model.Flavor = {
    if (f.isInstanceOf[gsd.iml.ast.flavor.NoneFlavor])
      return gsd.cdl.model.NoneFlavor

    if (f.isInstanceOf[gsd.iml.ast.flavor.BoolFlavor])
      return gsd.cdl.model.BoolFlavor

    if (f.isInstanceOf[gsd.iml.ast.flavor.DataFlavor])
      return gsd.cdl.model.DataFlavor ;

    return gsd.cdl.model.BoolDataFlavor ;
  }
}

/* . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

object ImlLegalValuesConstraintToCDLValuesOption {
  def apply(legalValues : LegalValuesConstraint) : LegalValuesOption =  {
    val ranges = List[Range]()
    val iterator = legalValues.getExpressions().iterator

    while(iterator.hasNext) {
      val exp = iterator.next
      if (exp.isInstanceOf[IntervalExpression])
        ranges.+:(ImlIntervalExpressionToMinMaxRange(exp.asInstanceOf[IntervalExpression]))
      else 
        ranges.+:(new SingleValueRange(ImlExpressionToCDLExpression(exp)))
    }    
    return new LegalValuesOption(ranges)
  }
}

/* . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

object ImlIntervalExpressionToMinMaxRange {
  def apply(e: IntervalExpression) : Range = {
      val interval = e.asInstanceOf[IntervalExpression] ;
      return new MinMaxRange(ImlExpressionToCDLExpression(interval.getFrom()),
                             ImlExpressionToCDLExpression(interval.getTo()))
  }
}

/* . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

object ImlConstraintsToCDLExpressions {
  def apply(constraints:java.util.List[_ <: UnaryImlConstraint]) : List[CDLExpression] = {
    var res = List[CDLExpression]()
    if (! gsd.iml.util.CollectionsUtils.isEmpty(constraints)) {
      val iterator = constraints.iterator
      while(iterator.hasNext) {
        res.+:(ImlExpressionToCDLExpression(iterator.next.getExpression()))
      }
    }
    return res
  }
}

/* . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

object ImlFeatureListToImlNodeList {
  def apply(features:java.util.List[Feature]) : List[Node] = {
    var res = List[Node]()
    if (! gsd.iml.util.CollectionsUtils.isEmpty(features)) {
      val iterator = features.iterator
      while(iterator.hasNext) {
        res.+:(ImlFeatureToNode(iterator.next))
      }
    }
    return res
  }
}


