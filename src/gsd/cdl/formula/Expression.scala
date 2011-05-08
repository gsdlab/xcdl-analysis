package gsd.cdl.formula

import TypeHelper._
import collection._
import ConditionalCompilation._

trait Error

class TypeError(expected:Type, oldType:Type, expr:GExpression) extends Error{
	assert((oldType & expected).isEmpty)
	override def toString() = expr + ":" + oldType + " cannot be converted to " + expected //+ "\nin " + expr.parentList.foldLeft("")((result, parent) => result + "\n\t" + parent)
	def expectedType = expected
}

class IdentitySet[T <: AnyRef] {
	import java.lang.System._
	private val data = mutable.Map[Int,mutable.ListBuffer[T]]()
	def +=(t:T) {
		val hash = identityHashCode(t)
		data.get(hash) match {
			case Some(list) => 
				val existing = list.foldLeft(false)((result, item)=>if (result) true else if (item eq t) true else false)
				if (!existing) list += t
			case None =>
				val list = new mutable.ListBuffer[T]()
				data.put(hash, list)
				list += t
		}
	}
	
	def ++=(ts:Traversable[T]) {
		for(t<-ts) this += t
	}
	
	def head():T = data.head._2.head
	
	def isEmpty = data.isEmpty
	
	def size = data.foldLeft(0)((sum, pair)=>sum + pair._2.size)
	
	def -=(t:T) {
		val hash = identityHashCode(t)
		data.get(hash) match {
			case Some(list) =>
				val (index, found) = list.foldLeft((0,false))({case ((index, found), item)=>if (found) (index, found) else if (item eq t) (index, true) else (index+1, false)})
				if (found) {
					list.remove(index)
					if (list.size == 0) data -= hash					
				}
			case None =>
		}
	}
}

object GExpressionHelper {
	var literal_counter = 0
	implicit def createVarRef(id:String) = GVariable(id)
	implicit def createIntLiteral(v:Int) = GLongIntLiteral(v)
	
	import org.kiama.rewriting.Rewriter._
	import GRewriter._
	val simplify:GExpression=>GExpression = expr => rewrite(geverywherebu(rulef(simplifySingle)))(expr)
	val removeGLiteral:GExpression=>GExpression = expr => rewrite(geverywherebu(rulef(removeGLiteralSingle)))(expr)
	val removeBooleanEq:GExpression=>GExpression = expr => rewrite(geverywherebu(rulef(removeBooleanEqSingle)))(expr)
	private def simplifySingle(t:Any):Any = t match {
		case GAnd(_, GBoolValue(false)) => GBoolLiteral(false)
		case GAnd(GBoolValue(false), _) => GBoolLiteral(false)
		case GAnd(GBoolValue(true), e) => e
		case GAnd(e, GBoolValue(true)) => e
		case GAnd(e1, e2) if e1 == e2 => e1
		case GOr(_, GBoolValue(true)) => GBoolLiteral(true)
		case GOr(GBoolValue(true), _) => GBoolLiteral(true)
		case GOr(GBoolValue(false), e) => e
		case GOr(e, GBoolValue(false)) => e
		case GOr(e1, e2) if e1 == e2 => e1
		case GConditional(GBoolValue(true), pass, fail) => pass
		case GConditional(GBoolValue(false), pass, fail) => fail
		case GNot(GBoolValue(true)) => GBoolLiteral(false)
		case GNot(GBoolValue(false)) => GBoolLiteral(true)
		case GPlus(GLongIntValue(0), e) => e
		case GPlus(e, GLongIntValue(0)) => e
		case GConditional(GBoolFunc(e1), e2, GBoolValue(false)) if e1 == e2 => e1
		case GConditional(e1, e2, GBoolValue(false)) if e1 == e2 => e1
		case GGreaterThan(GLongIntValue(i1), GLongIntValue(i2)) => 
			if (i1 > i2) return GBoolLiteral(true) 
			else return GBoolLiteral(false)
		case GGreaterEqThan(GLongIntValue(i1), GLongIntValue(i2)) => 
			if (i1 >= i2) return GBoolLiteral(true) 
			else return GBoolLiteral(false)
		case GLessThan(GLongIntValue(i1), GLongIntValue(i2)) => 
			if (i1 < i2) return GBoolLiteral(true) 
			else return GBoolLiteral(false)
		case GLessEqThan(GLongIntValue(i1), GLongIntValue(i2)) => 
			if (i1 <= i2) return GBoolLiteral(true) 
			else return GBoolLiteral(false)
		case GEq(GLongIntValue(i1), GLongIntValue(i2)) => 
			if (i1 == i2) return GBoolLiteral(true) 
			else return GBoolLiteral(false)
		case GSubString(GStringValue(whole), GStringValue(part)) =>
			if (whole.containsSlice(part)) return GBoolLiteral(true)
			else return GBoolLiteral(false)
		case x@_ => x
	}
	private def removeGLiteralSingle(t:Any):Any = t match {
		case l@GLiteral(v) if l.getType >= BoolType => GBoolLiteral(v > 0)
		case l@GLiteral(v) if l.getType >= IntType => GLongIntLiteral(v)
		case l@GLiteral(v) if l.getType >= StringType => GStringLiteral(v.toString)
		case x@_ => x
	}
	private def removeBooleanEqSingle(t:Any):Any = t match {
		case GEq(x, y) if x.getType() == BoolType && y.getType() == BoolType => 
			(x & y) | (GNot(x) & GNot(y))
		case x@_ => x
	}
		
		
	def removeSingleBooleanConditional(t:Any):Any = t match {
		case c@GConditional(b, p, f) if c.getType() == BoolType => (b & p) | (GNot(b) & f)
		case _ => t
	}

	def removeSingleDataConditional(t:Any):Any = {
		object ContainsConditional{
			def unapply(expr:GExpression):Option[GConditional] = {
				var r:Option[GConditional] = None
				val isConditional:PartialFunction[Any, GConditional] = { case x:GConditional if r == None => {r = Some(x);x} }
				geverywheretd(query(isConditional))(t)
				r
			}
		}
		def replaceSingleConditional(src:GConditional, tgt:GExpression):PartialFunction[Any, Any] = {
			case e:GConditional if e == src => tgt
		}
		def replaceConditional(src:GConditional, tgt:GExpression)(e:GExpression):GExpression = 
			rewrite(geverywheretd(rule(replaceSingleConditional(src, tgt))))(e)
		t match {
		case e:GExpression if e.getType() == BoolType && !e.children.exists(_.getType() == BoolType) => 
			e match {
				case ContainsConditional(c@GConditional(b, p, f)) => 
					(b & replaceConditional(c, p)(e)) | 
					(GNot(b) & replaceConditional(c, f)(e))
				case _ => e
			}
		case _ => t
		}
	}
	
	val removeConditional:GExpression=>GExpression = rewrite(geverywheretd(rulef(removeSingleBooleanConditional)) <* geverywheretd(rulef(removeSingleDataConditional))) _ 

}

object TypeGraph {
	
	def clear() { exprNodeMap.clear() }
	
	private class Node(val expr:GExpression) {
		val upstreamNodes = mutable.Set[Node]()
		var exprType:Type = allType
		lazy val childNodes = expr.children.map(getNode(_))
		override def toString = exprType.toString
	}
	
	override def toString() = {
		exprNodeMap.foldLeft(new StringBuilder())( 
			(result:StringBuilder, v:(GExpression, Node)) => 
			{val (expr, node) = v; result ++= expr + ":" + node.exprType + "\n";result}
		).toString
	}
	
	private val exprNodeMap = mutable.Map[GExpression, Node]()
	private def getNode(expr:GExpression):Node = {
		exprNodeMap.get(expr) match {
			case Some(n) => n
			case None => 
				val node:Node = new Node(expr)
				exprNodeMap.put(expr, node)
				for(child <- expr.children){
					val childNode = getNode(child)
					childNode.upstreamNodes += node
				}
				node
		}
	}
	def getType(expr:GExpression) = getNode(expr).exprType
	def setType(expr:GExpression, newType:Type) { getNode(expr).exprType = newType }
	def checkAndSetType(expr:GExpression, newType:Type):Option[TypeError] = {
		val oldType = getType(expr)
		if (oldType < newType)
			return Some(new TypeError(newType, oldType, expr))
		else {
			setType(expr, newType)
			return None
		}
	}
	
	def propagateType():List[TypeError] = {
		IF[(CompilationOptions.TRACE_TYPE_PROPAGATION)#v] {
			println("\n********* Expression Hierarchy *********")
			for ((expr, node) <- exprNodeMap)
			{
				println(expr)
				assert(expr == node.expr)
				node.upstreamNodes.foreach(n => println("\t" + n.expr))
			}
		}
		val toProcess = mutable.Set[Node]() 
		toProcess ++= exprNodeMap.values
		val errors = new mutable.ListBuffer[TypeError]()
		while(!toProcess.isEmpty) {
			val curNode = toProcess.head
			toProcess -= curNode
			IF[(CompilationOptions.TRACE_TYPE_PROPAGATION)#v] {
				println()
				println("**next:" + curNode.expr)
				println("oldTypes:" + (curNode.exprType::curNode.childNodes.map(_.exprType)).toString)
			}
			val oldSelfType = curNode.exprType
			val oldChildTypes = curNode.childNodes.map(_.exprType)
			val (selfType, childTypes) = curNode.expr.getDesiredTypes(oldSelfType, oldChildTypes)
			(oldSelfType::oldChildTypes).zip(selfType::childTypes).foreach(pair=>{val (oldType, newType) = pair; assert(newType <= oldType || (newType & oldType).isEmpty, "type propagation bug. " + curNode.expr + ", old types:" + oldSelfType::oldChildTypes + ", new types:" + selfType::childTypes)})
			assert(curNode.expr.getDesiredTypes(selfType, childTypes) == (selfType, childTypes))
			
			IF[(CompilationOptions.TRACE_TYPE_PROPAGATION)#v] {
				println("newTypes:" + (selfType::childTypes).toString)
			}
			
			def dealWithEachType(exprs:List[TypeGraph.Node], types:List[Type]) {
				if (exprs.size == 0){
					assert(types.size == 0)
					return
				}
				val oldType = exprs.head.exprType
				val newType = (types.head & oldType).getOrElse(types.head) //in case the expr appears twice and has already been changed
				if (oldType >= newType) {
					if (oldType != newType){
						exprs.head.exprType = newType
						IF[(CompilationOptions.TRACE_TYPE_PROPAGATION)#v] {
							println("##" + exprs.head.expr + " has changed to " + newType)
						}
						toProcess += exprs.head
						toProcess ++= exprs.head.upstreamNodes
					}
				}
				else {
					errors += new TypeError(newType, oldType, exprs.head.expr)
					println("Error 245: " + new TypeError(newType, oldType, exprs.head.expr))
					IF[(CompilationOptions.TRACE_TYPE_PROPAGATION)#v] {
						println("!!!error recorded")
					}
				}
				
				dealWithEachType(exprs.tail, types.tail)
			}

			dealWithEachType(curNode::curNode.childNodes, selfType::childTypes)			
			IF[(CompilationOptions.TRACE_TYPE_PROPAGATION)#v] {
				println("**newSet:" + toProcess.map(_.expr))
			}
		}
		errors.toList
	}

	
}

@serializable
abstract class GExpression() {
	
 	def children():List[GExpression] = 
		(for(i <- 0 until this.asInstanceOf[Product].productArity;
			child = this.asInstanceOf[Product].productElement (i);
			if (child.isInstanceOf[GExpression])) 
			yield child.asInstanceOf[GExpression]).toList

	def getDesiredTypes(oldType:Type, oldChildTypes:List[Type]) : (Type, List[Type])
	
	def & (that:GExpression):GExpression = GAnd(this, that)
	def | (that:GExpression):GExpression = GOr(this, that)
	def + (that:GExpression):GExpression = GPlus(this, that)
	def === (that:GExpression):GExpression = GEq(this, that)
	def <= (that:GExpression):GExpression = GLessEqThan(this, that)
	def < (that:GExpression):GExpression = GLessThan(this, that)
	def > (that:GExpression):GExpression = GGreaterThan(this, that)
	def unary_! = GNot(this)

	def getType() : Type
}

trait TypeStored extends GExpression{
	private var _type:Type = allType
	def getType() = _type
	def setType(t:Type) { _type = t }
}

abstract class GStaticTypedAtomicExpression(theType:Type) extends GExpression {
	def getDesiredTypes(oldType:Type, oldChildTypes:List[Type]) = {
		assert(oldChildTypes.size == 0)
		(theType, List())
	}
	override def getType() = theType
}

object GBoolValue {
	def unapply(expr:GExpression):Option[Boolean] = expr match {
		case GBoolLiteral(t) => Some(t)
		case GLiteral(0) => Some(false)
		case GLiteral(1) => Some(true)
		case GBoolFunc(GLongIntValue(0)) => Some(false)
		case GBoolFunc(GLongIntValue(1)) => Some(true)
		case _ => None
	}
}

object GLongIntValue {
	def unapply(expr:GExpression):Option[Long] = expr match {
		case GLongIntLiteral(t) => Some(t)
		case GLiteral(t) => Some(t)
		case _ => None
	}
}

object GStringValue {
	def unapply(expr:GExpression):Option[String] = expr match {
		case GLiteral(t) => Some(t.toString)
		case GStringLiteral(t) => Some(t)
		case _ => None
	}
}


/**
 * A literal can be of any types
 **/
case class GLiteral(value:Long/*, id:Int*/) extends GExpression with TypeStored {
	def getDesiredTypes(oldType:Type, oldChildTypes:List[Type]) = (oldType, List())
	override def toString = value.toString
	// def this(value:Int) = this(value, {GExpressionHelper.literal_counter+=1;GExpressionHelper.literal_counter})
	override def equals(arg:Any):Boolean = if (!arg.isInstanceOf[GLiteral]) false else this eq arg.asInstanceOf[GLiteral]
	override def hashCode():Int = java.lang.System.identityHashCode(this)
}
case class GStringLiteral(value:String) extends GStaticTypedAtomicExpression(StringType) {
	override def toString = "\"" + value + "\""
}
case class GLongIntLiteral(value:Long) extends GStaticTypedAtomicExpression(IntType) {
	override def toString = value.toString + ":LongInt"
}
case class GBoolLiteral(value:Boolean) extends GStaticTypedAtomicExpression(BoolType) {
	override def toString = if (value) "true" else "false"
}
// case class GVariableRef(variable:GVariable) extends GExpression() {
	// def getDesiredTypes() = (expectedType & variable.expectedType) match {
		// case Some(t) => (t, List(t))
		// case None => (expectedType, List(expectedType))
	// }
	// override def toString = variable.id
// }

class GVariableLike(id:String) extends GExpression  with TypeStored {
	def getDesiredTypes(oldType:Type, oldChildTypes:List[Type]) = {
		assert(oldChildTypes.size == 0);
		(oldType, List())
	}
	override def toString = id
}

case class GVariable(id:String) extends GVariableLike(id) {
	private var enums:scala.collection.mutable.Set[GExpression] = null

	def setEnums(enums: scala.collection.mutable.Set[GExpression]) = {
		this.enums = enums
	}

	def getEnums() : scala.collection.mutable.Set[GExpression] = {
		enums
	}

	def isEnum() = enums != null
}

/**
  * Enumeration Variable
  */
case class GEnumLiteral(originalValue:String, realValue:String) extends GVariableLike(originalValue) {

	override def toString = {
	 "enum_" + realValue
	}
}

case class GNot(inner:GExpression) extends GExpression() {
	def getDesiredTypes(oldType:Type, oldChildTypes:List[Type]) = (BoolType, List(BoolType))
	override def toString = "!" + inner
	override def getType() = BoolType
	
}


case class GConditional(cond:GExpression, pass:GExpression, fail:GExpression) extends GExpression {
	def getDesiredTypes(oldType:Type, oldChildTypes:List[Type]) = {
		val newType = findSmallestPossibleType(oldChildTypes.tail.head, oldChildTypes.tail.tail.head, oldType)
		(newType, List(BoolType, newType, newType))
	}
	
	override def toString = "(" + cond + " ? " + pass + " : " + fail + ")"
	override def getType() = (pass.getType() & fail.getType()).getOrElse(pass.getType)
}

abstract class GStaticTypedBinaryExpression(
	outputType:Type, 
	inputType:Type, 
	left:GExpression, 
	right:GExpression,
	operator:String="#") extends GExpression() {
	
	override def getDesiredTypes(oldType:Type, oldChildTypes:List[Type]) = (outputType, List(inputType, inputType))
	override def toString = "(" + left + ' ' + operator + ' ' + right + ")"
	override def getType() = outputType
	def getLeft :GExpression =  left
	def getRight :GExpression =  right
}

abstract class GStaticReturnBinaryExpression(
	outputType:Type,
	left:GExpression, 
	right:GExpression,
	operator:String="#") extends GExpression() {
	override def getDesiredTypes(oldType:Type, oldChildTypes:List[Type]) = {
		val commonType = findSmallestPossibleType(oldChildTypes.head, oldChildTypes.tail.head)
		(outputType, List(commonType, commonType))
	}
	
	override def toString = "(" + left + ' ' + operator + ' ' + right + ")"
	override def getType() = outputType
}
	

case class GOr(left:GExpression, right:GExpression) extends GStaticTypedBinaryExpression(BoolType, BoolType, left, right, "||") 
case class GAnd(left:GExpression, right:GExpression) extends GStaticTypedBinaryExpression(BoolType, BoolType, left, right, "&&")
case class GEq(left:GExpression, right:GExpression) extends GStaticReturnBinaryExpression(BoolType, left, right, "=")
case class GLessThan(left:GExpression, right:GExpression) extends GStaticTypedBinaryExpression(BoolType, IntType, left, right, "<")
case class GGreaterThan(left:GExpression, right:GExpression) extends GStaticTypedBinaryExpression(BoolType, IntType, left, right, ">")
case class GLessEqThan(left:GExpression, right:GExpression) extends GStaticTypedBinaryExpression(BoolType, IntType, left, right, "<=")
case class GGreaterEqThan(left:GExpression, right:GExpression) extends GStaticTypedBinaryExpression(BoolType, IntType, left, right, ">=")
case class GPlus(left:GExpression, right:GExpression) extends GStaticTypedBinaryExpression(IntType, IntType, left, right, "+")
case class GMinus(left:GExpression, right:GExpression) extends GStaticTypedBinaryExpression(IntType, IntType, left, right, "-")
case class GTimes(left:GExpression, right:GExpression) extends GStaticTypedBinaryExpression(IntType, IntType, left, right, "*")
case class GDivide(left:GExpression, right:GExpression) extends GStaticTypedBinaryExpression(IntType, IntType, left, right, "/")
case class GMod(left:GExpression, right:GExpression) extends GStaticTypedBinaryExpression(IntType, IntType, left, right, "%")
case class GDot(left:GExpression, right:GExpression) extends GStaticTypedBinaryExpression(StringType, StringType, left, right, ".")
case class GSubString(whole:GExpression, sub:GExpression) extends GStaticTypedBinaryExpression(BoolType, StringType, whole, sub, "contains")

case class GBoolFunc(inner:GExpression) extends GExpression() {
	override def getDesiredTypes(oldType:Type, oldChildTypes:List[Type]) = (BoolType, oldChildTypes)
	override def toString = "bool(" + inner + ")"
	override def getType() = BoolType
}

case class GOptionalBoolFunc(inner:GExpression) extends GExpression() {
	override def getDesiredTypes(oldType:Type, oldChildTypes:List[Type]) = 
		if (BoolType <= oldType)
			((oldType & (oldChildTypes.head | BoolType)).get, oldChildTypes)
		else {
			val newType = findSmallestPossibleType(oldType, oldChildTypes.head)
			(newType, List(newType))
		}
	override def toString = "optBool(" + inner + ")"
	override def equals(arg:Any):Boolean = if (!arg.isInstanceOf[GOptionalBoolFunc]) false else this eq arg.asInstanceOf[GOptionalBoolFunc]
	override def hashCode():Int = java.lang.System.identityHashCode(this)
	override def getType() = inner.getType() | BoolType
}


abstract class Type {
	private def getTypeSet(t:Type):Set[Type] = t match {
		case DisjunctiveType(s) => s
		case _ => Set(t)
	}
	
	def & (that:Type) : Option[Type] = {
		val thisTypes = getTypeSet(this)
		val thatTypes = getTypeSet(that)
		val newTypes:Set[Type] = thisTypes & thatTypes
		if (newTypes.size == 1) return Some(newTypes.head)
		else if (newTypes.size == 0) return None
		else return Some(DisjunctiveType(newTypes))
	}
	
	def | (that:Type) : Type = {
		val thisTypes = getTypeSet(this)
		val thatTypes = getTypeSet(that)
		val newTypes:Set[Type] = thisTypes | thatTypes
		if (newTypes.size == 1) return newTypes.head
		else return DisjunctiveType(newTypes)
	}
	
	def >= (that:Type) : Boolean = {
		val thisTypes = getTypeSet(this)
		val thatTypes = getTypeSet(that)
		return thatTypes.subsetOf(thisTypes)
	}
	
	def <= (that:Type) : Boolean = that >= this
	def > (that:Type) : Boolean = this >= that && this != that
	def < (that:Type) : Boolean = that > this

}
case object StringType extends Type {
	override def toString = "String"
}
/**
 * mnovakovic: Added enumeration type
 */
case object EnumType extends Type {
	override def toString = "Enumeration"
}
case object IntType extends Type {
	override def toString = "Int"
}
case object BoolType extends Type {
	override def toString = "Bool"
}
object TypeHelper {
	// implicit def Type2DisjunctiveType(s:Type):DisjunctiveType = {
		// s match {
		// case d:DisjunctiveType => d
		// case _ => new DisjunctiveType(Set(s))
		// }
	// }
	def findSmallestPossibleType(a:Type, b:Type, c:Type) = {
		(a & b) match {
			case Some(d) => 
				(d & c) match {
					case Some(e) => e
					case None => d
				}
			case None =>
				(a & c) match {
					case Some (d) => d
					case None => 
						(a & b) match {
							case Some(d) => d
							case None => a
						}
				}
		}
	}
	def findSmallestPossibleType(a:Type, b:Type) = {
		(a & b) match {
			case Some (c) => c
			case None => a
		}
	}
        /**
         * mnovakovic: Added EnumType
         */
	val allType = DisjunctiveType(Set(StringType, IntType, BoolType, EnumType))
}
case class DisjunctiveType(types:Set[Type]) extends Type{
	assert(types.size > 1)
		
	override def toString = "(" + types.tail.foldLeft(types.head.toString)(_ + "|" + _.toString) + ")"
}
