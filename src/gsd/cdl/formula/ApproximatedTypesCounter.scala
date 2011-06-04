package gsd.cdl.formula

class ApproximatedTypesCounter() {

  private var intType = 0
  private var stringType = 0
  private var boolType = 0
//  private var enumType = 0
  private var integerEnumType = 0
  private var stringEnumType = 0
  private var mixedEnumType = 0
  private var noneType = 0
  private var packageType = 0
  private var total = 0

  def addNone(amount:Int) = {
   noneType += amount
  }

  def addInt(amount:Int) = {
   intType += amount
  }

  def addBool(amount:Int) = {
   boolType += amount
  }

//  def addEnum(amount:Int) = {
//   enumType += amount
//  }

  def addIntegerEnum(amount:Int) = {
   integerEnumType += amount
  }

  def addStringEnum(amount:Int) = {
   stringEnumType += amount
  }

  def addMixedEnum(amount:Int) = {
   mixedEnumType += amount
  }

  def addPackage(amount:Int) = {
   packageType += amount
  }

  def addString(amount:Int) = {
   stringType += amount
  }

  def getStringCount():Int = {
   stringType
  }

  def getBoolCount():Int = {
   boolType
  }

  def getIntCount():Int = {
   intType
  }  
  
  def getPackageCount():Int = {
   packageType
  }

  def getNoneCount():Int = {
   noneType
  }

/*
  def getEnumCount():Int = {
   enumType
  }
*/

  def getStringEnumCount():Int = {
   stringEnumType
  }

  def getIntegerEnumCount():Int = {
   integerEnumType
  }

  def getMixedEnumCount():Int = {
   mixedEnumType
  }

  def getTotalCount():Int = {
   stringEnumType + integerEnumType + mixedEnumType +
   noneType + packageType + intType + boolType + stringType
  }

  override def toString() = {
   "Mixed enums: " + mixedEnumType + ", strings: " + stringType + 
   ", integers: " + intType + ", booleans: " + boolType
  }
}
