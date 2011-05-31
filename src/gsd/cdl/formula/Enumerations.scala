package gsd.cdl.formula

import gsd.cdl._
import gsd.cdl.model._
import collection._
import org.kiama.rewriting.Rewriter._
import gsd.cdl.parser.EcosIML

object Enumerations {

  private val TREAT_INTEGERS_ONLY_AS_ENUMS = true

  //mnovakovic - ENUMS
  // TODO: Calculated enumerations!
  def isEnumeration(node: Node): Boolean = {
    if (node.legalValues != None) {
      val Some(LegalValuesOption(ranges)) = node.legalValues
      (ranges.collect{case range: SingleValueRange => range}.
       filter(isCandidateForEnum).size > 0) // we have at least one string value
    } else if (node.calculated != None) {
     if (node.flavor != BoolFlavor) {
      isCalculatedEnum(node.calculated.getOrElse(null))
     } else {
      false
     }
    } else
      false
  }

  def isCalculatedEnum(calculated:CDLExpression):Boolean = {
    val list = listPossibleCalculatedExpressions(calculated)
    var numberOfStrings = 0
    var numberOfBools = 0
    var numberOfInts = 0
    list.foreach ( value => value match {
     case StringLiteral (v) => numberOfStrings += 1
     case LongIntLiteral (v) => {
        numberOfInts += 1
     }
     // True() represents bool value, TODO: This might be a hack. change
     case True () => numberOfBools += 1
     case _ => throw new Exception ("Only booleans, strings and integers alowed as enumerands")
    })
    if (numberOfBools > 0 && (numberOfStrings + numberOfInts) > 0) {
//     println("Warning! Calculated cannot have both boolean and non-boolean values at the same time: " + calculated)
     false
    } else {
     // mnovakovic: this is where we turn on/off treating all-integers as enumerations
     if (TREAT_INTEGERS_ONLY_AS_ENUMS) {
      (numberOfStrings > 0 || numberOfInts > 0)
     } else {
      numberOfStrings > 0
     }
    }
  }
  /**
   * A SingleValueRange is a candidate for enum if
   * it's list of values has at least one String
   */
  def isCandidateForEnum(expr:SingleValueRange) : Boolean = {
    expr.v match {
      case StringLiteral(v) =>
              val numberPattern = """0[xX]([0123456789abcdefABCDEF]+)""".r
              numberPattern.unapplySeq(v) match {
                      case Some(List(numberPart)) => {
                            true
                      }
                      // hexa-decad number as string
                      case None => {
                        if (TREAT_INTEGERS_ONLY_AS_ENUMS) {
                          true
                        } else {
                          false
                        }
                      }
            }
      // Conditional will always return either String or Int
      // TODO: In theory, it can also return boolean, which is an error, 
      // but should we check for that?!?
      case Conditional(cond, pass, fail) => true
      case LongIntLiteral(v) => {
        if (TREAT_INTEGERS_ONLY_AS_ENUMS) {
          true
        } else {
          false
        }
      }
      case _ => false
    }
  }

  def listPossibleValues(node: Node): scala.collection.mutable.Set[String] = {
    if (node.legalValues != None) {
     var set = scala.collection.mutable.Set[String]()
          val Some(LegalValuesOption(ranges)) = node.legalValues
          ranges.foreach((range:Range) => {
              set = set ++ listPossibleLegalValues(range.asInstanceOf[SingleValueRange].v)
          });
     set
    } else if (node.calculated != None) {
     listPossibleCalculatedValues(listPossibleCalculatedExpressions(node.calculated.getOrElse(null)))
    } else
      throw new Exception("This node is not Enumeration!")
  }

  private def listPossibleCalculatedValues(exps:scala.collection.mutable.Set[CDLExpression]):scala.collection.mutable.Set[String] = {
   var set = scala.collection.mutable.Set[String]()
   exps.foreach (
    exp => {
     exp match {
      case StringLiteral(v) => set += v
      case LongIntLiteral(v) => set += "" + v
      case _ => throw new Exception("Only Strings and Ints allowed")
     }
    }
   )
   set
  }

    private def listPossibleCalculatedExpressions(exp:CDLExpression):scala.collection.mutable.Set[CDLExpression] = {
     var map = scala.collection.mutable.Set[CDLExpression]()
     exp match {
       case LongIntLiteral(v) => map += exp
       case StringLiteral(v) => map += exp
       case Conditional(cond, pass, fail) => {
           val leftExpClass = map = map ++ listPossibleCalculatedExpressions(pass).toList
           val rightExpClass = map = map ++ listPossibleCalculatedExpressions(fail).toList
  //         if (!leftExpClass.getClass.equals(rightExpClass.getClass)) 
  //           throw new Exception("Types don't match")
  //         leftExpClass
       }
       case Dot(left, right) =>  map += True()
       case Div(left, right) =>  map += True()
       case Times(left, right) =>  map += True()
       case Plus(left, right) =>  map += True()
       case And(left, right) => map += True()
       case Eq(left, right) => map += True()
       case Or(left, right) => map += True()
       case Not(op) => map += True()
       case Identifier(id) => map += True()
       case _ => println("Unknown operator for enumeration: " + exp.toString); false
       // throw new Exception("Unknown operator for enumeration: " + exp.toString)
     }
     map
    }

    private def listPossibleLegalValues(exp: CDLExpression):scala.collection.mutable.Set[String] = {
//      var enums = mutable.Map[String, mutable.Set[GExpression]]()
      var set = scala.collection.mutable.Set[String]()
      exp match {
				    case StringLiteral(v) => {
//          if (v != "") {
//			       enums.apply(n.id) += GEnumLiteral(Utils.guardEnumValue(v), "" + Utils.guardEnumValue(v) + "_" + n.id)
          set += Utils.guardEnumValue(v)
        }

        case LongIntLiteral(v) => {
          set += "" + v
//						    enums(n.id) += GEnumLiteral("" + v, "" + v + "_" + n.id)
        }

        case Conditional(cond, pass, fail) => {
            set = set ++ listPossibleLegalValues(pass)
            set = set ++ listPossibleLegalValues(fail)
//          enums = enums ++ handleEnums(pass, n)
//          enums = enums ++ handleEnums(fail, n)
        }

        case _ => throw new Exception("Enumeration should be of type StringLiteral: " + exp)
      }   
//      enums 
      set
    }


}
