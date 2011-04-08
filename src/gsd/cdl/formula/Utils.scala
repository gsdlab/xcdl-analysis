package gsd.cdl.formula

object Utils {
  def guardEnumValue(value:String): String = {
//		if (value.startsWith("\"") && value.endsWith("\"")) {
//			value.substring(1, value.length - 1)
//		} else
//			value
    value.replaceAll("\"", "")
  }
}
