package gsd.cdl.formula

/**
 * Describes enumeration types
 */
object EnumerationTypes {
   val INTEGERS = 0
   val STRINGS = 1
   val COMBINED = 2
   val UNDEFINED = -1
}

/**
 * Contains info about enumerands of a feature
 */
class Enumerands() {
 
 var set = scala.collection.mutable.Set[GExpression]()

 var enumType = EnumerationTypes.UNDEFINED

 def getEnumerands() = {
  set
 }
 
 def += (exp:GEnumLiteral) = {
  set += exp
  if (isInteger(exp)) {
   if (enumType == EnumerationTypes.UNDEFINED) {
    enumType = EnumerationTypes.INTEGERS
   } else if (enumType == EnumerationTypes.STRINGS) {
    enumType = EnumerationTypes.COMBINED
   }
  } else {
   if (enumType == EnumerationTypes.UNDEFINED) { 
    enumType = EnumerationTypes.STRINGS
   } else if (enumType == EnumerationTypes.INTEGERS) {
    enumType = EnumerationTypes.COMBINED
   }
  }
 }

 private def isInteger(exp:GEnumLiteral):Boolean = {
  try {
   val i = Integer.valueOf(exp.originalValue.trim)
   true
  } catch {
   case _ => {
//         println("exception: " + exp)
       false
   }
  }
 }
}

