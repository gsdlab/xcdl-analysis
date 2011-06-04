/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package gsd.cdl.formula

import java.io.File
import java.io.FileFilter
import gsd.cdl.parser.EcosIML
import gsd.cdl.model._
import org.kiama.rewriting.Rewriter._

object ApproximatedTypes {
  
  def main(args:Array[String]) = {
     val out = new java.io.FileWriter("highlevel_approximatedTypes.csv")

     val fs = new File("input/iml/").listFiles(new FileFilter() {
        def accept(pathname:File) = pathname.getName.endsWith(".iml")
     })

     out.write("filename;booleans;integers;strings;booldataflavors;pure_integers\n")

     for(f <- fs) {
        printFileResults(f, out)
     }
     out.close()
  }

  def printFileResults (f: File, out:java.io.FileWriter) = {
      print("*** File " + f.getName + "\n")
      gsd.cdl.formula.TypeGraph.clear()

      val (bools, ints, strings, booldatas, pure_integers) = getApproximatedTypesCount(f.getAbsolutePath)
      
      out.write(f.getName + ";" + bools + ";" + ints + ";"  + strings + ";" + 
                booldatas + ";" + pure_integers + "\n")

      out.flush
  }

/**
1. Integer + String Enumerations
2. Ranges
3. Boolean Flavors
4. Yingfei's for what is not done
*/

  def getApproximatedTypesCount(file:String) = {
    import scala.collection._
    val nodes = EcosIML.parseFile(file)
    // let this function populate all vars and everything    
    val (variables, constraints, errors) = Main.convertToGeneralModel(nodes)
    val enums = Main.produceEnumerations(EcosIML.parseFile(file))

    val allNodes:List[Node] = collectl { case n:Node => n }(nodes).map(_ match {
      case Node(id, PackageType, display, description, flavor, defaultValues, calculated, legalValues, reqs, activeIfs, implements, children) => Node(id, PackageType, display, description, NoneFlavor, defaultValues, calculated, legalValues, reqs, activeIfs, implements, children)
      case x@_ => x
    })

    val allNodesMap = allNodes.map(n => (n.id, n)).toMap[String, Node]


//    println("NEW CONSTRAINTS OUTPUT")
//    var counter = new ApproximatedTypesCounter()

//    println("Nodes size: " + allNodesMap.size + ", printing")
    
    var inferrencedVariableNames = mutable.Set[String]()
    var allIdentifierNames = mutable.Set[String]()
    var allPureIntegersNames = mutable.Set[String]()
    var allIntegerNames = mutable.Set[String]()
    var allStringNames = mutable.Set[String]()
//    var allMixedNames = mutable.Set[String]()
    var allBooleanNames = mutable.Set[String]()
    var allBoolDataFeatureNames = mutable.Set[String]()

    var allEnumNames = mutable.Set[String]()

    allEnumNames ++= enums.keys

    allNodesMap.keys.foreach(value => {
     allIdentifierNames += value;
    })

    enums.foreach(value => {
      if (value._2.enumType == EnumerationTypes.INTEGERS) {
       allPureIntegersNames += value._1
       allIntegerNames += value._1
      } else if (value._2.enumType == EnumerationTypes.STRINGS) {
       allStringNames += value._1
      } else {
//       allMixedNames += value._1
       allStringNames += value._1
      }
    })    

    allNodes.foreach(node => {
     node.legalValues match {
      case Some(legalValues) => {
       legalValues.ranges.foreach (value => {
        var minMax = 0
        var single = 0
        var minValueInRange:Option[Long] = None
        var maxValueInRange:Option[Long] = None
        value match {
         case MinMaxRange(min, max) => {
          minMax += 1
          try{
           minValueInRange = Some(min.asInstanceOf[LongIntLiteral].value)
          } catch {
           case e => {} //println(min);println(max)
          }

          try{
           maxValueInRange = Some(max.asInstanceOf[LongIntLiteral].value)
          } catch {
           case e => {} //println(min);println(max)
          }
          
          if (minValueInRange != None || maxValueInRange != None) {
           allIntegerNames += node.id.trim
           allPureIntegersNames += node.id.trim
          }
         }
         case SingleValueRange(v) => {single += 1}
        }
        if (minMax > 0 && single > 0)
         throw new Exception("Both MinMax and Single in one expression")
        if (minMax > 1)
         throw new Exception("Two MinMaxs defined")
       })
      }
      case None => {}
     }
    })

    allIdentifierNames.foreach(value => {
      val node = allNodesMap.apply(value)
      // add bool flavors as Booleans
      if (node.flavor == BoolDataFlavor || node.cdlType == PackageType) {
       allBoolDataFeatureNames += value
      }

      if (node.cdlType == PackageType) {
       allStringNames += value
      }

      if (node.flavor == BoolFlavor || (node.flavor == NoneFlavor && node.cdlType != PackageType)
          ) {
       allBooleanNames += value
      }

      if (node.cdlType == InterfaceType) {
       if (node.flavor != BoolFlavor) {
        allIntegerNames += value
       }
      }
    })

    variables.keys.foreach(value => {
     if (value.endsWith("_bool_var")) {
      if (!variables.keys.toList.contains(value.replaceAll("_bool_var", "_data_var")) && 
        !variables.keys.toList.contains(value.replaceAll("_bool_var", "_scalar_var"))
      ) {
         if (!allBooleanNames.contains(value.replaceAll("_bool_var", "")))
         println("Warning: Identifier: " + value.replaceAll("_bool_var", "") + 
              " is seen as boolean, but it does not have the BoolFlavor")
      }
      inferrencedVariableNames += value.replaceAll("_bool_var", "")
     } else if (value.endsWith("_data_var")) {
      inferrencedVariableNames += value.replaceAll("_data_var", "")
     } else if (value.endsWith("_scalar_var")) {
      inferrencedVariableNames += value.replaceAll("_scalar_var", "")
     } else {
      throw new Exception(value)     
     }

    })

   var inferencedTypes = mutable.Map[String, Option[String]]()

   // integer enums
   allIntegerNames.foreach(value => {inferencedTypes += (value -> Some("integer"));})

   // string enums
   allStringNames.foreach(value => {inferencedTypes += (value -> Some("string")); })

//   mixed enums
//   allMixedNames.foreach(value => {inferencedTypes += (value -> Some("string")); })

   // definitive boolean types
   allBooleanNames.foreach(value => {inferencedTypes += (value -> Some("bool"))})

    inferrencedVariableNames.foreach(value => {
       var valueName = value + "_bool_var"
       if (!variables.contains(valueName)) {
         valueName = value + "_data_var"
         if (!variables.contains(valueName)) {
           valueName = value + "_scalar_var"
         }
       } else {
         // bool_data
         if (variables.contains(value + "_data_var")) {
           valueName = value + "_data_var"
         }

         if (variables.contains(value + "_scalar_var")) {
           valueName = value + "_scalar_var"
         }
       }

       variables.apply(valueName).getType match {
        case IntType => {
         if (!inferencedTypes.contains(value)) {
          inferencedTypes += (value -> Some("integer"))
          allIntegerNames += value
         } else {
//          println("Already influenced: " + value + " with value: " + inferencedTypes.apply(value))
         }
        }
        case StringType => {
         if (!inferencedTypes.contains(value)) {
          inferencedTypes += (value -> Some("string"))
          allStringNames += value
         } else {
//          println("Already influenced: " + value + " with value: " + inferencedTypes.apply(value))
         }
        }
        case DisjunctiveType(types) => {
         if (!inferencedTypes.contains(value)) {
           if (isFeaturePotentiallyNumber(value)) {
             inferencedTypes += (value -> Some("integer"))
             allIntegerNames += value
           } else {
             inferencedTypes += (value -> Some("string"))
             allStringNames += value
           }
         } else {
//          println("Already influenced: " + value + " with value: " + inferencedTypes.apply(value))
         }
        }
        case _ => {}
       }
      }
    )

    // add uninferred, and put them as a string
    allIdentifierNames.filter(!inferencedTypes.contains(_)).foreach(value => {
       if (isFeaturePotentiallyNumber(value)) {
         inferencedTypes += (value -> Some("integer"))
         allIntegerNames += value
       } else {
         inferencedTypes += (value -> Some("string"))
         allStringNames += value
       }
    })
    
    allBoolDataFeatureNames.foreach(value => allBooleanNames += value)

/*
    println("Total size: " + allIdentifierNames.size)
    println("Boolean features: " + allBooleanNames.size)
    println("Integer features: " + allIntegerNames.size)
    println("String features: " + allStringNames.size)
    println("BoolData features: " + allBoolDataFeatureNames.size)
*/

    if (allIdentifierNames.size != (allBooleanNames.size + 
     allIntegerNames.size + allStringNames.size - allBoolDataFeatureNames.size)) {
     println("PROBLEM in file!: " + file)
    }
    (allBooleanNames.size, allIntegerNames.size, allStringNames.size, 
     allBoolDataFeatureNames.size, allPureIntegersNames.size)
  }

  def isFeaturePotentiallyNumber(id:String):Boolean = {
    val v = scala.collection.mutable.Set[String]()
    v += "SIZE"
    v += "TICKS"
    v += "COUNT"
    v += "PORT"
    v += "SPEED"
    v += "DENOMINATOR"
    v += "NUMERATOR"
    v += "PERIOD"
    v += "RATE"
    v += "CLOCK"
    if (v.filter(id.contains(_)).size > 0) {
     true
    } else {
     false
    }
  }

}


/*
  def getApproximatedTypesCount(file:String):ApproximatedTypesCounter = {
    import scala.collection._
    // let this function populate all vars and everything    
    val (variables, constraints, errors) = Main.convertToGeneralModel(EcosIML.parseFile(file))
    val enums = Main.produceEnumerations(EcosIML.parseFile(file))
//    println("NEW CONSTRAINTS OUTPUT")
    var counter = new ApproximatedTypesCounter()

//    println("Nodes size: " + allNodesMap.size + ", printing")
    
    var inferrencedVariableNames = mutable.Set[String]()
    var allIdentifierNames = mutable.Set[String]()
    var allTypableIdentifierNames = mutable.Set[String]()
    var allNoneFlavors = mutable.Set[String]()
    var allPackages = mutable.Set[String]()
    var allNonBooleanInterfaces = mutable.Set[String]()
    var allIntegerEnumNames = mutable.Set[String]()
    var allStringEnumNames = mutable.Set[String]()
    var allMixedEnumNames = mutable.Set[String]()
    var allBooleanNames = mutable.Set[String]()

    var allEnumNames = mutable.Set[String]()


//    allEnumNames ++= enums.keys
    allEnumNames ++= enums.keys

    enums.foreach(value => {
      if (value._2.enumType == EnumerationTypes.INTEGERS) {
       allIntegerEnumNames += value._1
      } else if (value._2.enumType == EnumerationTypes.STRINGS) {
       allStringEnumNames += value._1
      } else {
       allMixedEnumNames += value._1
      }
    })

    allNodesMap.keys.foreach(value => {
     allIdentifierNames += value;
    })

    allIdentifierNames.foreach(value => {
      val node = allNodesMap.apply(value)
      if (node.flavor == NoneFlavor) {
         if (node.cdlType == PackageType) {
           allPackages += value
         } else {
           allNoneFlavors += value;
         }
      } else {
         allTypableIdentifierNames += value;
      }
      // add bool flavors as Booleans
      if (node.flavor == BoolFlavor) {
       allBooleanNames += value
      }
      if (node.cdlType == InterfaceType) {
       if (node.flavor != BoolFlavor) {
        allNonBooleanInterfaces += value
       }
      }
    })

    variables.keys.foreach(value => {
     if (value.endsWith("_bool_var")) {
      if (!variables.keys.toList.contains(value.replaceAll("_bool_var", "_data_var")) && 
        !variables.keys.toList.contains(value.replaceAll("_bool_var", "_scalar_var"))
      ) {
         if (!allBooleanNames.contains(value.replaceAll("_bool_var", "")))
         println("Warning: Identifier: " + value.replaceAll("_bool_var", "") + 
              " is seen as boolean, but it does not have the BoolFlavor")
      }
      inferrencedVariableNames += value.replaceAll("_bool_var", "")
     } else if (value.endsWith("_data_var")) {
      inferrencedVariableNames += value.replaceAll("_data_var", "")
     } else if (value.endsWith("_scalar_var")) {
      inferrencedVariableNames += value.replaceAll("_scalar_var", "")
     } else {
      throw new Exception(value)     
     }

    })

   var differenceInferedNotInfered = allTypableIdentifierNames -- inferrencedVariableNames.toList
   differenceInferedNotInfered = differenceInferedNotInfered -- allEnumNames.toList
   differenceInferedNotInfered = differenceInferedNotInfered -- allNonBooleanInterfaces.toList
   differenceInferedNotInfered = differenceInferedNotInfered -- allBooleanNames.toList

   //println("Unique inferenced variables size: " + inferrencedVariableNames.size)
   //println("Unique indentifiers size: " + allIdentifierNames.size)
   //println("Unique typable size: " + allTypableIdentifierNames.size)
   //println("Unique UNtypable size: " + allNoneFlavors.size)
   //println("Difference: " + differenceInferedNotInfered.size)
   //differenceInferedNotInfered.foreach(println)
   //println("Enums size: " + allEnumNames.size)

   var inferencedTypes = mutable.Map[String, Option[String]]()
   // first add enumerations 
//   allEnumNames.foreach(value => {inferencedTypes += (value -> Some("enum"))})
//   counter.addEnum(allEnumNames.size)

   // integer enums
   allIntegerEnumNames.foreach(value => {inferencedTypes += (value -> Some("integer_enum"));})
   counter.addIntegerEnum(allIntegerEnumNames.size)

   // string enums
   allStringEnumNames.foreach(value => {inferencedTypes += (value -> Some("string_enum")); })
   counter.addStringEnum(allStringEnumNames.size)

   // mixed enums
   allMixedEnumNames.foreach(value => {inferencedTypes += (value -> Some("mixed_enum")); })
   counter.addMixedEnum(allMixedEnumNames.size)

   // then add uninferred, and put them as a string
   differenceInferedNotInfered.foreach(value => {inferencedTypes += (value -> Some("string"))})
   counter.addString(differenceInferedNotInfered.size)

   //interfaces that are not booleans are int
   allNonBooleanInterfaces.foreach(value => {inferencedTypes += (value -> Some("int"))})
   counter.addInt(allNonBooleanInterfaces.size)

   // definitive boolean types
   allBooleanNames.foreach(value => {inferencedTypes += (value -> Some("bool"))})
   counter.addBool(allBooleanNames.size)

   // add none flavors
   allNoneFlavors.foreach(value => {inferencedTypes += (value -> Some("none"))})
   counter.addNone(allNoneFlavors.size)

   // add packages
   allPackages.foreach(value => {inferencedTypes += (value -> Some("package"))})
   counter.addPackage(allPackages.size)

    inferrencedVariableNames.foreach(value => {
       var valueName = value + "_bool_var"
       if (!variables.contains(valueName)) {
         valueName = value + "_data_var"
         if (!variables.contains(valueName)) {
           valueName = value + "_scalar_var"
         }
       } else {
         // bool_data
         if (variables.contains(value + "_data_var")) {
           valueName = value + "_data_var"
         }

         if (variables.contains(value + "_scalar_var")) {
           valueName = value + "_scalar_var"
         }
       }
       variables.apply(valueName).getType match {
        case IntType => {
         if (!inferencedTypes.contains(value)) {
          inferencedTypes += (value -> Some("int"))
          counter.addInt(1)
         } else {
//          println("Already influenced: " + value + " with value: " + inferencedTypes.apply(value))
         }
        }
        case StringType => {
         if (!inferencedTypes.contains(value)) {
          inferencedTypes += (value -> Some("string"))
          counter.addString(1) 
         } else {
//          println("Already influenced: " + value + " with value: " + inferencedTypes.apply(value))
         }
        }
        case DisjunctiveType(types) => {
         if (!inferencedTypes.contains(value)) {
           if (value.contains("SIZE") || value.contains("COUNT") || 
             value.contains("PORT") || value.contains("PORT")) {
             inferencedTypes += (value -> Some("int"))
             counter.addInt(1)
           } else {
             inferencedTypes += (value -> Some("string"))
             counter.addString(1)
           }
         } else {
//          println("Already influenced: " + value + " with value: " + inferencedTypes.apply(value))
         }
        }
        case _ => {}
       }
      }
    )

    if (inferencedTypes.size != counter.getTotalCount || counter.getTotalCount != allIdentifierNames.size) {
     println("PROBLEM in file!: " + file)
    }
//    (allIdentifierNames.toList.diff(inferencedTypes.keys.toList)).foreach(println)
    
    counter
  }

*/
