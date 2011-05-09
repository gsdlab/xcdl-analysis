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
package gsd.cdl.parser

import gsd.cdl.parser.adapter.ImlFeatureListToImlNodeList
import gsd.cdl.model.Node
import gsd.iml.parser.ImlParser 

object EcosIML {

  def parseFile(file : String) : List[Node] =  {
    val original = ImlParser.parse(file)
    println("original size " + original.size)
    return ImlFeatureListToImlNodeList(original)
  }

  def main(args:Array[String]) : Unit = parseFile("C:/Users/leonardo/Documents/NetBeansProjects/xcdl-analysis-dev_1.1/test/gsd/cdl/formula/showcase")
}
