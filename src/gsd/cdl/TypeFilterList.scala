/*
 * Copyright (c) 2009 Steven She <shshe@gsd.uwaterloo.ca>
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

trait TypeFilterList {
  class SuperList[A <: AnyRef](lst : List[A]) {
    //Uses experimental Scala Manifests to reify types
    def typeFilter[T](implicit m:scala.reflect.Manifest[T]) =
      lst.filter{x:A => m.erasure.isInstance(x)}.asInstanceOf[List[T]]

    def sortByType =
      lst.sortWith{(x,y) => x.getClass.getName < y.getClass.getName }
  }

  implicit def toSuperList[A <: AnyRef](lst : List[A]) = new SuperList[A](lst)
}

object TypeFilterListConversions extends TypeFilterList