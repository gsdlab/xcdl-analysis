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

case class Node(id : String, 
                cdlType : CDLType,
                display : String,
                description : Option[String],
                flavor : Flavor,
                defaultValue : Option[CDLExpression], //TODO restrict to only literals
                calculated : Option[CDLExpression],
                legalValues : Option[LegalValuesOption],
                reqs : List[CDLExpression],
                activeIfs : List[CDLExpression],
                implements : List[CDLExpression], // just identifiers
                children : List[Node])

sealed abstract class Flavor
case object NoneFlavor extends Flavor{
  override def toString() = "none"
}
case object BoolFlavor extends Flavor{
  override def toString() = "bool"
}
case object DataFlavor extends Flavor{
  override def toString() = "data"
}
case object BoolDataFlavor extends Flavor{
  override def toString() = "booldata"
}

sealed abstract class CDLType
case object OptionType extends CDLType{
  override def toString() = "option"
}
case object ComponentType extends CDLType{
  override def toString() = "component"
}
case object PackageType extends CDLType{
  override def toString() = "package"
}
case object InterfaceType extends CDLType{
  override def toString() = "interface"
}

