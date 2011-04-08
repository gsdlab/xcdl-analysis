/*
 * Copyright (c) 2010 Steven She <shshe@gsd.uwaterloo.ca>
 * and Thorsten Berger <berger@informatik.uni-leipzig.de>
 *
 * This file is part of CDLTools.
 *
 * CDLTools is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * CDLTools is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with CDLTools.  If not, see <http://www.gnu.org/licenses/>.
 */
package gsd.cdl

import model._

import TypeFilterListConversions._
import util.parsing.input.PagedSeqReader
import collection.immutable.PagedSeq
import util.parsing.combinator.{ImplicitConversions, JavaTokenParsers}

trait IMLParser extends CDLExpressionParser with ImplicitConversions {

  implicit def toOption(o : Option[ExpressionOption]) = o match {
    case Some(x) => Some(x.expr)
    case None => None
  }

  implicit def toExpressionList(lst : List[ExpressionOption]) =
    lst.map { _.expr }

  abstract class ExpressionOption(val expr : CDLExpression) extends CDLOption
  case class ActiveIfOption(e : CDLExpression) extends ExpressionOption(e)
  case class DefaultOption(e : CDLExpression) extends ExpressionOption(e)
  case class RequiresOption(e : CDLExpression) extends ExpressionOption(e)
  case class CalculatedOption(e : CDLExpression) extends ExpressionOption(e)
  case class ImplementsOption(s : CDLExpression) extends ExpressionOption(s)


  lazy val cdl = rep(node)
  lazy val node :Parser[Node] = cdlType ~ identifier ~ ("{" ~>
            opt(display) ~ rep(option) ~ opt(flavor) ~ opt(description) ~ rep(node) <~
           "}") ^^
      {
        case (ty~Identifier(id))~(optDisp~opts~optFlav~desc~children) =>
//          println("Begin processing: " + id)

          //0..* options
          val activeIfs = opts.typeFilter[ActiveIfOption]
          val reqs = opts.typeFilter[RequiresOption]
          val implements = opts.typeFilter[ImplementsOption]
          //0..1 options
          val disp = optDisp getOrElse ""
          val defVal = opts.typeFilter[DefaultOption].headOption
          val calc = opts.typeFilter[CalculatedOption].headOption
          val flav = optFlav.getOrElse {
            ty match {
              case ComponentType | OptionType => BoolFlavor
              case InterfaceType => DataFlavor
              case PackageType => BoolDataFlavor
            }
          }
          
          //Assumes there is only one legal_values declared
          val legalValues = opts.typeFilter[LegalValuesOption].headOption
//          println(legalValues)

          Node(id, ty, disp, desc, flav, defVal, calc, legalValues, reqs, activeIfs, implements, children)
      }

  lazy val cdlType =
      "option" ^^^ OptionType |
      "component" ^^^ ComponentType |
      "package" ^^^ PackageType |
      "interface" ^^^ InterfaceType

  lazy val display = "display" ~> lit ^^ { case StringLiteral(v) => v }
  lazy val description = "description" ~> lit ^^ { case StringLiteral(v) => v }

  lazy val flavor = "flavor" ~>
       ("none" ^^^ NoneFlavor |
        "data" ^^^ DataFlavor |
        "bool(data)?".r ^^ {
          case "bool" => BoolFlavor
          case "booldata" => BoolDataFlavor
       })

  lazy val option = defaultValue | activeIf | req | calculated | implements | legalValues
  lazy val defaultValue = "default_value" ~> "[" ~> expr <~ "]" ^^ DefaultOption
  lazy val activeIf = "active_if" ~> "[" ~> expr <~ "]" ^^ ActiveIfOption
  lazy val req = "requires" ~> "[" ~> expr <~ "]" ^^ RequiresOption
  lazy val calculated = "calculated" ~> "[" ~> expr <~ "]" ^^ CalculatedOption
  lazy val implements = "implements" ~> "[" ~> identifier <~ "]" ^^ ImplementsOption
  lazy val legalValues =
    "legal_values" ~> "[" ~> rep(range) <~ "]" ^^ LegalValuesOption

  lazy val range = expr ~ opt("to" ~> expr) ^^ {
    case e~None => SingleValueRange(e)
    case e1~Some(e2) => MinMaxRange(e1, e2)
  }
}

object EcosIML extends IMLParser {

  def parseString(s : String) =
    parseAll(cdl, s) match {
      case Success(res,_) => res
      case x => error(x.toString)
    }

  def parseFile(file : String) =
    parseAll(cdl, new PagedSeqReader(PagedSeq fromFile file)) match {
      case Success(res,_) => res
      case x => error(x.toString)
    }

  def main(args : Array[String]) : Unit = {
    parseFile(args(0))
  }
}
