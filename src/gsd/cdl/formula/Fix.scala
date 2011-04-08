package gsd.cdl.formula

import collection._
import org.kiama.rewriting.Rewriter._
import GRewriter._


object FixSystem {
	type PropositionMap = Map[GProposition, GExpression]
	val PropPrefix = "___P___"
	
	case class GProposition(nid:Int) extends GVariableLike(PropPrefix + nid){
		setType(BoolType)
	}
	
	def toPropositional(expressions:Seq[GExpression]):(Seq[GExpression], PropositionMap) = {
		var counter:Int = 0
		val propositionMap = mutable.Map[GProposition, GExpression]()
		val inverseMap = mutable.Map[GExpression, GProposition]()
		def toPropositionalSingle():PartialFunction[Any, GExpression] = {
			case e:GExpression if e.getType() == BoolType && e.children.exists(e => !(BoolType <= e.getType())) => {
				val oldVar = inverseMap.get(e)
				if (oldVar.isDefined) {
					oldVar.get
				}
				else {
					val newVar = GProposition(counter)
					counter += 1
					propositionMap.put(newVar, e)
					inverseMap.put(e, newVar)
					newVar
				}
			}
		}
		val resultExprs = expressions.map(rewrite(geverywheretd(rule(toPropositionalSingle))))
		(resultExprs, propositionMap)
	}
	
	def buildVarConstraintMap(pConstraints:Array[GExpression], propositionMap:PropositionMap):Map[GVariableLike, Set[Int]] = {
		val result = mutable.Map[GVariableLike, Set[Int]]()
		def updateRefSingle(c:Int)():PartialFunction[Any, Unit] = {
			case p:GProposition => 
				updateRef(c)(propositionMap(p))
			case v:GVariable => 
				val exprSet = result.get(v).getOrElse(Set[Int]())
				result.put(v, exprSet + c)
		}
		def updateRef(c:Int)(e:GExpression) = rewrite(geverywheretd(query(updateRefSingle(c))))(e) 
		for (i <- 0 until pConstraints.size) updateRef(i)(pConstraints(i))
		
		propositionMap.foreach(pair => {
			val (prop, expr) = pair
			assert(result.get(prop).isEmpty)
			result.put(prop, Set())
			def updatePropSingle(p:GProposition)():PartialFunction[Any, Unit] = {
				case v:GVariable => {
					val propSet = result(p)
					result.put(p, propSet ++ result(v))
				}
			}
			rewrite(geverywheretd(query(updatePropSingle(prop))))(expr)
		})
		result
	}
	
	def simplify(fixes:Traversable[BooleanFix]):List[BooleanFix] = {
		var result = List[BooleanFix]()
		fixes.foreach( f => {
			def filter(result:List[BooleanFix]):List[BooleanFix] = 
				if (result.isEmpty) List(f)
				else if (result.head <= f) result
				else if (f <= result.head) filter(result.tail)
				else result.head::(filter(result.tail))
			result = filter(result)
		})
		assert(result.toSet.size == result.size)
		assert(result.toSet subsetOf fixes.toSet)
		result
	}


	def times(fs1:Traversable[BooleanFix], fs2:Traversable[BooleanFix]):List[BooleanFix] = {
		val result = 
			(for{f1 <- fs1
				f2 <- fs2
				if (!(f1 conflict f2))}
				yield f1 ++ f2)
		simplify(result)
	}

	def getSingleFix(target:GExpression, expected:Boolean=true):List[BooleanFix] = target match {
		case GNot(e) => getSingleFix(e, !expected)
		case v:GVariableLike => { 
			assert(v.getType == BoolType)
			List(BooleanFix(Map(v->expected)))
		}
		case GBoolValue(v) => if (v == expected) List(BooleanFix(Map())) else List()
		case GAnd(e1, e2) => 
			if (expected) {
				val fs1:List[BooleanFix] = getSingleFix(e1, true)
				val fs2:List[BooleanFix] = getSingleFix(e2, true)
				times(fs1, fs2)
			} 
			else {
				getSingleFix(GOr(e1, e2), true)
			}
		case GOr(e1, e2) =>
			if (expected) simplify(getSingleFix(e1, true) ++ getSingleFix(e2, true))
			else getSingleFix(GAnd(e1, e2), true)
	}

	
	def buildVarConstraintMap2(pConstraints:Array[GExpression], propositionMap:PropositionMap) = propositionMap

	def getAllVariablesC(constraint:GExpression, propositionMap:PropositionMap):Set[GVariable] = {
		val result = collectl {
			case v:GVariable => Set(v)
			case v:GProposition => getAllVariablesC(propositionMap(v), propositionMap)
		}(constraint)
		Set[GVariable]() ++ result.flatten(x=>x)
	}
	def getAllVariables(fix:BooleanFix, propositionMap:PropositionMap):Set[GVariable] = {
		def toVariables(v:GVariableLike):Set[GVariable] = v match {
			case v:GVariable => Set(v)
			case v:GProposition => getAllVariablesC(propositionMap(v), propositionMap)
		}
		// val a = fix
		// val b = a.literals
		// val c = b.keySet
		// c.map(toVariables).flatten(x=>x)
		fix.literals.keySet.map(toVariables).flatten(x=>x)
	}

	val _relatedConstraints1 = mutable.Set[Int]()
	val _relatedConstraints2 = mutable.Set[Int]()
	def getBooleanFix2(targetIndex:Int, 
		allSatisfiedConstraints:Iterable[Int],
		propositionMap:PropositionMap,
		allConstraints:Array[GExpression]):List[BooleanFix] = {

		var fixes = getSingleFix(allConstraints(targetIndex))		
		for ( constraintIndex <- allSatisfiedConstraints ) {
			val constraint = allConstraints(constraintIndex)
			var cfixes = getSingleFix(constraint) // This can be optimized by a prefetching of fixes for all constraints
			fixes = fixes.map( fix =>
				if (!(getAllVariables(fix, propositionMap) intersect getAllVariablesC(constraint, propositionMap)).isEmpty) {
					_relatedConstraints2 += constraintIndex
					times(List(fix), cfixes)
				}
				else List(fix)
			).flatten(x=>x)
		}
		fixes
	}
	
	def getBooleanFix(targetIndex:Int, 
		allSatisfiedConstraints:Iterable[Int],
		varConstraintMap:Map[GVariableLike, Set[Int]],
		allConstraints:Array[GExpression]):List[BooleanFix] = {
		assert(!allSatisfiedConstraints.exists(_ == targetIndex))
		val target = allConstraints(targetIndex)
		
		def simplify(fixes:List[BooleanFix]):List[BooleanFix] = {
			var result = List[BooleanFix]()
			fixes.foreach( f => {
				def filter(result:List[BooleanFix]):List[BooleanFix] = 
					if (result.isEmpty) List(f)
					else if (result.head <= f) result
					else if (f <= result.head) filter(result.tail)
					else result.head::(filter(result.tail))
				result = filter(result)
			})
			assert(result.toSet.size == result.size)
			assert(result.toSet subsetOf fixes.toSet)
			result
		}
		val fixes = getSingleFix(target)
		
		def timeRelatedFixes(fix:BooleanFix, remainConstraints:Set[Int], size:Int):List[BooleanFix] = {
			// if (size % 100 == 0) println(size)
			val relatedConstraints:Set[Int] = fix.literals.keySet.map(varConstraintMap(_)).flatten
			val effectiveRelatedConstraints:Set[Int] = relatedConstraints intersect remainConstraints
			if (effectiveRelatedConstraints.size == 0) return List(fix)
			_relatedConstraints1 ++= effectiveRelatedConstraints
			val c = effectiveRelatedConstraints.head
			def replaceSingle:PartialFunction[Any, Any] = {
				case v:GVariableLike if fix.literals.contains(v) => GBoolLiteral(fix.literals(v))
			}
			val replacedC = rewrite(geverywheretd(rule(replaceSingle)))(allConstraints(c))
			val cFixes:List[BooleanFix] = getSingleFix(replacedC)
			val newFixes = simplify(cFixes.map(_ ++ fix))
			var newSize = size + newFixes.size - 1
			val result = for (fix <- newFixes) yield {
				val ret = timeRelatedFixes(fix, remainConstraints - c, newSize)
				newSize += ret.size - 1
				ret
			}
			result.flatten
		}
		
		simplify(fixes.map(fix => timeRelatedFixes(fix, Set() ++ allSatisfiedConstraints, 0)).flatten(x=>x))
	}
	
	def booleanToDataFix(
		fix:BooleanFix, 
		propositionMap:PropositionMap, 
		configuration:Map[GVariable, Any]):List[DataFix] = {
			val propositions = fix.literals.keySet.filter(_.isInstanceOf[GProposition])
			val variables = propositions.map(getAllVariablesC(_, propositionMap)).flatten(x=>x)
			List()
		}
	
	def mergeDataFix(fixes:Traversable[DataFix]):List[DataFix] = List()
	
	def analyze(constraints:Seq[GExpression]) {
		val (replacedConstraints, propositionMap) = toPropositional(constraints)
		val allConstraints = replacedConstraints.toArray
		val varConstraintMap = buildVarConstraintMap(allConstraints, propositionMap)
		val fixCount = (0 until allConstraints.size).map(i => {
			val fixes = getSingleFix(allConstraints(i));
			(	i, 
				fixes.size, 
				if (fixes.size == 0) 0 else fixes.map(getAllVariables(_, propositionMap).size).max,
				if (fixes.size == 0) 0 else fixes.map(getAllVariables(_, propositionMap).filter(_.getType != BoolType).size).max
			)
		})
		fixCount.sortBy(x => x._2).foreach(i => println((i._1 + 1) + ":" + i._2 + ":" + i._3 + ":" + i._4))
		// val fixes = getBooleanFix(4, Set() ++ (0 until allConstraints.size) - 4, varConstraintMap1, allConstraints)
		// println("relatedConstraints1: " + _relatedConstraints1)
		// println("relatedConstraints2: " + _relatedConstraints2)
		// for (c <- 0 until allConstraints.size) {
			// val fixes = getBooleanFix2(c, Set() ++ (0 until allConstraints.size) - c, varConstraintMap, allConstraints)
			// fixes.foreach(f => println(f))
		// }
	}
	
	
}

case class BooleanFix(literals:Map[GVariableLike, Boolean]) {
	def conflict(that:BooleanFix):Boolean = {
		literals.exists( pair => {
			val (thisVar, thisPositive) = pair
			val thatPositive = that.literals.get(thisVar)
			thatPositive.isDefined && thatPositive.get != thisPositive
		})
	}
	def ++ (that:BooleanFix) = BooleanFix(literals ++ that.literals)
	def <= (that:BooleanFix) = {
		literals.forall( pair => {
			val (thisVar, thisPositive) = pair
			val thatPositive = that.literals.get(thisVar)
			thatPositive.isDefined && thatPositive.get == thisPositive
		})
	}
}

case class DataFix(vars:Set[GVariable], constraint:GExpression)