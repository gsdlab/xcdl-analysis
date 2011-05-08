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
package gsd.cdl.model

object CDLExpressionConversions{
  implicit def string2Identifier(str : String) = Identifier(str)
}

abstract class CDLOption
case class LegalValuesOption(ranges : List[Range]) extends CDLOption
abstract class Range
case class MinMaxRange(low: CDLExpression, high: CDLExpression) extends Range
case class SingleValueRange( v : CDLExpression ) extends Range

sealed abstract class CDLExpression{
  def & (other : CDLExpression) : CDLExpression = other match {
    case True() => this
    case _ => And(this, other)
  }
  def | (other : CDLExpression) : CDLExpression = other match {
    case False() => this
    case _ => Or(this, other)
  }
  def unary_! = Not(this)
  def implies (other : CDLExpression) : CDLExpression = Implies(this, other)

  def splitConjunctions : List[CDLExpression] = this match {
    case And(x,True()) => x.splitConjunctions
    case And(True(),y) => y.splitConjunctions
    case And(x,y) => x.splitConjunctions ++ y.splitConjunctions
    case x => List(x)
  }
 
  def children():List[CDLExpression] =  
	(for(i <- 0 until this.asInstanceOf[Product].productArity;
		child = this.asInstanceOf[Product].productElement (i);
		if (child.isInstanceOf[CDLExpression])) 
		yield child.asInstanceOf[CDLExpression]).toList
 

}

case class NonBoolean( e : CDLExpression ) extends CDLExpression

case class StringLiteral(value : String) extends CDLExpression {
//  override def toString = value
//  override def toString = "\"" + value + "\""
}
case class LongIntLiteral(value : Long) extends CDLExpression {
//  override def toString = "" + value
}
case class Identifier(id : String) extends CDLExpression {
  override def toString = id
}

case class Conditional(cond : CDLExpression,
                       pass : CDLExpression,
                       fail : CDLExpression) extends CDLExpression

sealed abstract class UnaryExpression(e : CDLExpression,
                                      op : String) extends CDLExpression {
  override def toString = op + e
}

sealed abstract class BinaryExpression(l : CDLExpression,
                                       r : CDLExpression,
                                       op : String) extends CDLExpression {
  override def toString = "(" + l + " "  + op + " " + r + ")"
}

case class Or(left : CDLExpression, right : CDLExpression)
        extends BinaryExpression(left, right, "||")
case class And(left : CDLExpression, right : CDLExpression)
        extends BinaryExpression(left, right, "&&")

case class Eq(left : CDLExpression, right : CDLExpression)
        extends BinaryExpression(left, right, "==")
case class NEq(left : CDLExpression, right : CDLExpression)
        extends BinaryExpression(left, right, "!=")

case class LessThan(left : CDLExpression, right : CDLExpression)
        extends BinaryExpression(left, right, "<")
case class LessThanOrEq(left : CDLExpression, right : CDLExpression)
        extends BinaryExpression(left, right, "<=")
case class GreaterThan(left : CDLExpression, right : CDLExpression)
        extends BinaryExpression(left, right, ">")
case class GreaterThanOrEq(left : CDLExpression, right : CDLExpression)
        extends BinaryExpression(left, right, ">=")

case class Plus(left :CDLExpression, right : CDLExpression)
        extends BinaryExpression(left, right, "+")
case class Minus(left :CDLExpression, right : CDLExpression)
        extends BinaryExpression(left, right, "-")
case class Dot(left : CDLExpression, right : CDLExpression)
        extends BinaryExpression(left, right, ".")

case class BtAnd(left : CDLExpression, right : CDLExpression)
        extends BinaryExpression(left, right, "&")
case class BtOr(left : CDLExpression, right : CDLExpression)
        extends BinaryExpression(left, right, "|")
case class BtXor(left : CDLExpression, right : CDLExpression)
        extends BinaryExpression(left, right, "^")
case class BtLeft(left : CDLExpression, right : CDLExpression)
        extends BinaryExpression(left, right, "<<")
case class BtRight(left : CDLExpression, right : CDLExpression)
        extends BinaryExpression(left, right, ">>")

case class Times(left :CDLExpression, right : CDLExpression)
        extends BinaryExpression(left, right, "*")
case class Div(left :CDLExpression, right : CDLExpression)
        extends BinaryExpression(left, right, "/")
case class Mod(left :CDLExpression, right : CDLExpression)
        extends BinaryExpression(left, right, "%")

case class MinusMinus(expr : CDLExpression)
        extends UnaryExpression(expr, "--")
case class Not(expr : CDLExpression)
        extends UnaryExpression(expr, "!")

case class FunctionCall(name : String, arguments : List[CDLExpression]) extends CDLExpression{
  override def toString() = name + "(" + arguments.foldLeft("")( (a,b) => a + ( if( a!="" ) "," else "" ) + b ) + ")"
}


// added for boolean transformation
case class Implies(left : CDLExpression, right:CDLExpression)
      extends BinaryExpression(left, right, "->")

case class True() extends CDLExpression{
  override def & (other: CDLExpression) = other
  override def implies (other: CDLExpression) = other
  override def toString = "TRUE"
}
case class False() extends CDLExpression{
  override def | (other : CDLExpression) = other
  override def toString = "FALSE"
}
