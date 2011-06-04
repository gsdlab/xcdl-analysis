package gsd.cdl.formula

object Utils {
  def removeTrailingQuotesFrom(string:String):String = {
    if (string.charAt(0) == '"' && string.charAt(string.length - 1) == '"') {
      string.substring(1, string.length - 1)
    } else {
      string
    }
  }
  def guardEnumValue(value:String): String = {
//		if (value.startsWith("\"") && value.endsWith("\"")) {
//			value.substring(1, value.length - 1)
//		} else
//			value
    value.replaceAll("\"", "")
  }
  def guardEnumValue(value:Int): String = {
    guardEnumValue("" + value)
  }
}
