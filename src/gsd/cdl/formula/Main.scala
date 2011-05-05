package gsd.cdl.formula

import gsd.cdl._
import gsd.cdl.model._
import collection._
import org.kiama.rewriting.Rewriter._
import TypeHelper._
import GRewriter._
import ConditionalCompilation._
import java.io._


object CompilationOptions {
	/**
         * Should be true. 
         */
        type CREATE_LEGAL_VALUES_CONSTRAINTS = TRUE
	/**
         * If FALSE, then:
         *  A IMPL I
         *  B IMPL I
         *  constraint: I <= 1
         *  it transforms to:
         *  (A ? 1 : 0) + (B? 1 : 0) <= 1
         * If TRUE, transforms Interfaces to propositional logic:
         * (A AND not B) AND ...
         */
        type TRANSFORM_INTERFACE = TRUE
	/**
         * if TRUE = change all the conditions to
         * propositional logic
         * Should be false.
         */
        type REMOVE_CONDITIONAL = FALSE
	/**
         * If TRUE:
         * If you have a = b it becomes 
         * (a AND b) OR (not a AND not b)
         */
        type REMOVE_BOOLEAN_EQ = TRUE
	type TRACE_TYPE_PROPAGATION = FALSE
	type PRINT_INTERFACE_USAGE = FALSE
}


object Main {

	class CycleError(involvedIds:Set[String]) extends Error {
		override def toString = ("Cycle detected. Ids involved:" + involvedIds)
	}

	class NonExistingIdsError(id:String) extends Error {
		override def toString = "Refer to non-existing id:" + id
	}
	
	/**
	* Represents a constraint that will be simplified to false. 
	* In actual model, this means that 
	* a configuration cannot be exported because this constraint will never be satisfied
	*/
	case class UnsatisfiableConstraintError(exp:GExpression) extends Error {
		override def toString = "Expression causing the model to be unsat: " + exp.toString
	}

	/**
	* Represents a "dead feature" - a feature that will never be active.
	*/
	case class DeadFeatureError(id:String) extends Error {
		override def toString = id + " is a dead feature"
	}

	case class DuplicatedIDs(id:String) extends Error {
		override def toString = "Duplicated id: " + id
	}

	def parseFile(file:String):(Map[String, GVariable], List[GExpression], List[Error]) = convertToGeneralModel(EcosIML.parseFile(file))
	
	def main(args:Array[String]) {
		import java.io._

    println("main being executed...")

    println("parsing the file...")
    val nodes = EcosIML.parseFile(args(0))
    val (vars, constraints, activeConstraints, effectiveConstraints, errors) = convertToGeneralModelFull(nodes)
    val out = new java.io.FileWriter("result.log")
    out.write("*******Vars*****\n")
    vars.foreach(pair => out.write(pair._1 + ":" + TypeGraph.getType(pair._2) + "\n"))
    out.write("*******Constraints*****\n")
    constraints.foreach(c=> out.write(c.toString + "\n"))
    out.write("*******Errors*****\n")
    errors.foreach(e=> out.write(e + "\n"))
    out.close

    val serialization = new ObjectOutputStream(new FileOutputStream("result.data"))
    serialization.writeObject(vars)
    serialization.writeObject(constraints)
    serialization.close
	}

  def depth( id : String, childParentMap : Map[String,String] ): Int =
    childParentMap.get( id ) match {
      case Some(n) => depth( n, childParentMap ) + 1
      case None => 0
    }

  def outputHistogram( model : CDLModel, values : List[Int], file : String ){
		val pw = new PrintWriter( new File( file ) )
		aggregateValuesForGnuplot( values, 1 ).
            foreach( x => pw.println( x._1 + "," + x._2 ) )
		pw.close();
	}

  def aggregateValuesForGnuplot( values : List[Int], raster : Int ) = {
    val ret = mutable.Map[Int, Int]()
    for(f <- values ){
      val newValue:Int = ( ( f / raster ) * raster ) + raster/2;
      ret.get( newValue ) match{
        case None => ret + (newValue -> 1)
        case Some(v) => ret + ( newValue -> ( v + 1 ) )
      }
    }
		Map[Int,Int]() ++ ret
	}
        /**
         * Node is enumeration if
         * It's legal values are only list of strings
         */
        def isEnumeration(node: Node): Boolean = {
          if (node.legalValues != None) {
            val Some(LegalValuesOption(ranges)) = node.legalValues
            (ranges.collect{case range: SingleValueRange => range}.
             filter(isCandidateForEnum).size == ranges.size)
          } else
            false
        }

        /**
         * A SingleValueRange is a candidate for enum if
         * it is not an integer vlaue, i.e. Only Strings are enums
         */
        def isCandidateForEnum(expr:SingleValueRange) : Boolean = {
                expr.v match {
                    case StringLiteral(v) =>
                            val numberPattern = """0[xX]([0123456789abcdefABCDEF]+)""".r
                            numberPattern.unapplySeq(v) match {
                                    case Some(List(numberPart)) => false
                                    case None => {
                                          true
                                     }
                          }
                    case _ => false

                }
        }
	
	def reverse[K,V](m:Iterable[(K,V)]):Map[V,List[K]] = {
		val result = mutable.Map[V, List[K]]()
		for((k,v) <- m) {
			result.get(v) match {
				case Some(collection) => result.put(v, k::collection)
				case None => result.put(v, List(k))
			}
		}
		result.toMap
	}
	
	def reverseBack[K,V](m:Iterable[(V, Iterable[K]) ]):Map[K,V] = {
		val result = for((v, list) <- m; k <- list ) yield (k, v)
		return result.toMap
	}

  def convertToGeneralModel(nodes:Seq[Node], output:String=>Unit = print):(Map[String, GVariable], List[GExpression], List[Error]) = {
    val (vars, constraints, activeConstraints, effectiveConstraints, errors) = convertToGeneralModelFull(nodes)
    (vars, constraints, errors)
  }
	
	
	def convertToGeneralModelFull(nodes:Seq[Node], output:String=>Unit = print):(Map[String, GVariable], List[GExpression], Map[String, GExpression], Map[String, GExpression], List[Error]) = {
		var aliases = mutable.Map[String, GExpression]()
		val variables = mutable.Map[String, GVariable]()
		val errors = mutable.ListBuffer[Error]()

    val enumVariables = mutable.HashSet[String]()
    /**
     * key   = node_id
     * value = set of enumerated values
     */
    val enums = mutable.Map[String, mutable.Set[GExpression]]()

		
		case class GAliasReference(id:String) extends GExpression {
			def getDesiredTypes(oldType:Type, oldChildTypes:List[Type]) = (oldType & oldChildTypes.head) match {
				case Some(t) => (t, List(t))
				case None => (oldType, List(oldType))
			}
			override def toString = "alias:"+id
			override def children() = List(aliases.get(id) match {
				case Some(e) => e
				case None => 
					val replacement = new GLiteral(0)
					errors += new NonExistingIdsError(id)
					aliases.put(id, replacement)
					replacement
			} )
			
			override def getType() = aliases.get(id) match {
				case Some(e) => e.getType
				case None => allType
			}
		}
		def createRefData(id:String) = GAliasReference(id+"_refData")

		var initialTypes = mutable.Map[GExpression, Type]()
		def setType(expr:GExpression, newType:Type) {
			if (initialTypes.getOrElse(expr, allType) < newType) {
                          println ("error 140")
                          errors += new TypeError(newType, initialTypes(expr), expr)
                        }
			else initialTypes.put(expr, newType)
		}

               /*
                * Recreates the list of nodes. The flavor of all package nodes are
                * set to NoneFlavor.
                */
		val allNodes:List[Node] = collectl { case n:Node => n }(nodes).map(_ match {
				case Node(id, PackageType, display, description, flavor, defaultValues, calculated, legalValues, reqs, activeIfs, implements, children) => Node(id, PackageType, display, description, NoneFlavor, defaultValues, calculated, legalValues, reqs, activeIfs, implements, children)
				case x@_ => x
			} )

                /*
                 * Creates a map from each node id to the node itself.
                 */
		val allNodesMap:Map[String, Node] = allNodes.map(n => (n.id, n)).toMap[String, Node]

                allNodes.foreach(n => {
                    if (isEnumeration(n)) {
                      enums += n.id -> scala.collection.mutable.HashSet[GExpression]()
                      val Some(LegalValuesOption(ranges)) = n.legalValues
                      ranges.foreach((range:Range) => {
                          range.asInstanceOf[SingleValueRange].v match {
														case StringLiteral(v) => {
																	// if this enumeration has been encountered, add id to it
																	if (enumVariables.contains(Utils.guardEnumValue(v))) {
                                    // TODO: get rid of all quotes in guardEnumValue function																		
																		val newEnumName = Utils.guardEnumValue(v) + "_" + n.id 
																		enumVariables += newEnumName
																		enums(n.id) += GEnumLiteral(Utils.guardEnumValue(v), newEnumName) 
// CDL2GEnumExpression(StringLiteral(newEnumName))
																		//throw new Exception(Utils.guardEnumValue(v))
																	} else {
                                    enumVariables += Utils.guardEnumValue(v)
																		enums(n.id) += GEnumLiteral(Utils.guardEnumValue(v), Utils.guardEnumValue(v))
// CDL2GEnumExpression(StringLiteral(Utils.guardEnumValue(v)))
																	}
                                }
                            case _ => throw new Exception("Enumeration should be of type StringLiteral!")
                          }
                      });
                    }
                })

//								// mnovakovic
//                def CDL2GEnumExpression(expr : CDLExpression):GExpression = {
//									expr match {
//                          case StringLiteral(v) => {GEnumVariable(Utils.guardEnumValue(v))}
//                          case _ => throw new Exception("Cannot convert to Enum variable" + expr)
//                  }
//                }

		def CDL2GExpression(expr:CDLExpression):GExpression = {
			expr match {
				case StringLiteral(v) => 
                                      if (enumVariables.contains(Utils.guardEnumValue(v))) {
//                                       val result = new GStringLiteral(v)
//                                       if (enumVariables.contains(v)) {
//                                         setType(result, EnumType)
//                                       } else {
//                                         setType(result, EnumType | StringType)
//                                       }
//                                       result
                                        new GEnumLiteral(Utils.guardEnumValue(v), Utils.guardEnumValue(v))
                                      } else {
					val numberPattern = """0[xX]([0123456789abcdefABCDEF]+)""".r
					numberPattern.unapplySeq(v) match {
						case Some(List(numberPart)) => GIntLiteral(java.lang.Long.parseLong(numberPart, 16).toInt)
						case None => {
                                                      new GStringLiteral(v)
                                                 }
					}
                                      }
				case IntLiteral(v) =>
					if (v == 0) new GLiteral(v)
					else if (v == 1) {
						val result = new GLiteral(v)
						setType(result, IntType | BoolType)
						result
					}
					else GIntLiteral(v)
				case Identifier(v) => createRefData(v)
				case Conditional(cond, pass, fail) => GConditional(CDL2GExpression(cond), CDL2GExpression(pass), CDL2GExpression(fail))
				case Or(left, right) => GOr(CDL2GExpression(left), CDL2GExpression(right))
				case And(left, right) => GAnd(CDL2GExpression(left), CDL2GExpression(right))
				case Eq(left, right) => GEq(CDL2GExpression(left), CDL2GExpression(right))
				case NEq(left, right) => GNot(GEq(CDL2GExpression(left), CDL2GExpression(right)))
				case LessThan(left, right) => GLessThan(CDL2GExpression(left), CDL2GExpression(right))
				case LessThanOrEq(left, right) => GLessEqThan(CDL2GExpression(left), CDL2GExpression(right))
				case GreaterThan(l, r) => GGreaterThan(CDL2GExpression(l), CDL2GExpression(r))
				case GreaterThanOrEq(l, r) => GGreaterEqThan(CDL2GExpression(l), CDL2GExpression(r))
				case Plus(l,r)=>GPlus(CDL2GExpression(l),CDL2GExpression(r))
				case Minus(l,r)=>GMinus(CDL2GExpression(l),CDL2GExpression(r))
				case Dot(l,r)=>GDot(CDL2GExpression(l),CDL2GExpression(r))
				case Times(l,r)=>GTimes(CDL2GExpression(l),CDL2GExpression(r))
				case Div(l,r)=>GDivide(CDL2GExpression(l),CDL2GExpression(r))
				case Mod(l,r)=>GMod(CDL2GExpression(l),CDL2GExpression(r))
				case Not(e) => GNot(CDL2GExpression(e))
				case FunctionCall("is_substr", List(whole, sub)) => GSubString(CDL2GExpression(whole), CDL2GExpression(sub))
				case FunctionCall("bool", List(e)) => GBoolFunc(CDL2GExpression(e))
				case True() => GBoolLiteral(true)
				case False() => GBoolLiteral(false)
				case _ => throw new Exception("unexpected expression:" + expr)
			}
		}

		output("converting to general model...\n")
                
		/* Creates a map from each node towards its parent. */
		val parents:Map[String, String] = reverseBack(allNodes.map(n => (n.id, n.children.map(_.id))))

                /* Creates a map from each interfaces towards the node identifiers that implement it. */
                val interfaceImpl:Map[String, Seq[String]] = reverse(for (n <- allNodes; Identifier(i) <- n.implements) yield (n.id, i))

                def getImpl(id:String) = interfaceImpl.get(id) match {
			case Some(seq) => seq
			case None => List[String]()
		}
		
		//check duplicated ids
		val ids = mutable.Set[String]()
		allNodes.foreach(n => 
			if (ids.contains(n.id)) {
				errors += new DuplicatedIDs(n.id)
				ids // have to return ids to make it pass. really strange
			}
			else {
				ids += n.id
			}
		)
		// create variables, n_effective and n_data for non-interface
		for(node <- allNodes if node.cdlType != InterfaceType) { 
			node match {
				case Node(id,_,_,_,NoneFlavor,_,None,_,_,_,_,_) => 
					aliases.put(id+"_effective", GAliasReference(id + "_active"))
					aliases.put(id+"_data", new GLiteral(1))
				case Node(id,_,_,_,BoolFlavor,_,None,_,_,_,_,_) => 
					val nv_bool = GVariable(id+"_bool_var")
					setType(nv_bool, BoolType)
					variables.put(id+"_bool_var", nv_bool)
					aliases.put(id+"_effective", GAnd(nv_bool, GAliasReference(id+"_active")))
					aliases.put(id+"_data", nv_bool)
				case Node(id,_,_,_,BoolFlavor,_,Some(cl),_,_,_,_,_) =>
					val newCl = CDL2GExpression(cl)
					setType(newCl, BoolType)
					aliases.put(id+"_effective", GAnd(GAliasReference(id+"_active"), newCl))
					aliases.put(id+"_data", newCl)
				case Node(id,_,_,_,BoolDataFlavor,_,None,_,_,_,_,_) =>
					// TODO: Deal with BoolDataFlavor enumerations
          if (isEnumeration(node)) {
						val nv_bool = GVariable(id+"_bool_var")
						setType(nv_bool, BoolType)
						val nv_data = GVariable(id+"_scalar_var")
						setType(nv_data, StringType | IntType | EnumType)
            nv_data.setEnums(enums.get(id).getOrElse(null))
            variables.put(nv_bool.id, nv_bool)
						variables.put(nv_data.id, nv_data)
						aliases.put(id+"_effective", GAnd(GAliasReference(id+"_active"), nv_bool))
						aliases.put(id+"_data", nv_data)
          } else {
						val nv_bool = GVariable(id+"_bool_var")
						setType(nv_bool, BoolType)
						val nv_data = GVariable(id+"_data_var")
						setType(nv_data, StringType | IntType)
						variables.put(nv_bool.id, nv_bool)
						variables.put(nv_data.id, nv_data)
						aliases.put(id+"_effective", GAnd(GAliasReference(id+"_active"), nv_bool))
						aliases.put(id+"_data", nv_data)
				  }
				case Node(id,_,_,_,BoolDataFlavor,_,Some(cl),_,_,_,_,_) =>
					// TODO: What if cl is enumeration?
					val newCl = CDL2GExpression(cl)
					setType(newCl, StringType | IntType)
					aliases.put(id+"_effective", GAliasReference(id+"_active") & GBoolFunc(newCl))
					aliases.put(id+"_data", newCl)
				case Node(id,_,_,_,DataFlavor,_,None,_,_,_,_,_) =>
          if (isEnumeration(node)) {
            val nv_data = GVariable(id+"_scalar_var")
						//setType(nv_data, EnumType)
            nv_data.setEnums(enums.get(id).getOrElse(null))
            variables.put(nv_data.id, nv_data)
            aliases.put(id+"_effective", GAliasReference(id+"_active"))
            aliases.put(id+"_data", nv_data)
          } else {
            val nv_data = GVariable(id+"_data_var")
            setType(nv_data, StringType | IntType)
            variables.put(nv_data.id, nv_data)
            aliases.put(id+"_effective", GAliasReference(id+"_active"))
            aliases.put(id+"_data", nv_data)
          }
				case Node(id,_,_,_,DataFlavor,_,Some(cl),_,_,_,_,_) =>
					// TODO: What if cl is enumeration?
					val newCl = CDL2GExpression(cl)
					setType(newCl, StringType | IntType)
					aliases.put(id+"_effective", GAliasReference(id+"_active"))
					aliases.put(id+"_data", newCl)
				case _ => throw new Exception("unexpected node combination" + node)
			}
		}
		
		def boolCDL2GExpression(e:CDLExpression):GExpression = e match {
			case Identifier(_) => GOptionalBoolFunc(CDL2GExpression(e))
			case _ => CDL2GExpression(e)
		}
		
		//create n_active, n_refData
		for(node <- allNodes) {
			val aiPart = node.activeIfs.map(boolCDL2GExpression(_)).reduceLeftOption((left, right)=> GAnd(left, right))
			aiPart match {
				case Some(e) => setType(e, BoolType)
				case None => 
			}
			val parentPart = parents.get(node.id).map(nid=>new GAliasReference(nid + "_effective"))
			val conjunctionList = List() ++ aiPart ++ parentPart //++ boolDataPart
			val node_active = 
				if (conjunctionList.size == 0) 
					new GBoolLiteral(true)
				else
					conjunctionList.reduceLeft((l,r)=>GAnd(l,r))
			aliases.put(node.id + "_active", node_active)
			
			val refData = GConditional(GAliasReference(node.id+"_effective"), GAliasReference(node.id+"_data"), new GLiteral(0))
			aliases.put(node.id+"_refData", refData)
		}
		
		//create n_data, n_effective for interfaces
		for(node <- allNodes if node.cdlType == InterfaceType) {
			val id = node.id
			val n_booleanValue:GExpression = getImpl(id).foldLeft[GExpression](GBoolLiteral(false))((c:GExpression,id:String)=> c | GAliasReference(id+"_effective"))
			node match {
				case Node(_,_,_,_,BoolFlavor,_,_,_,_,_,_,_) =>
					aliases.put(id+"_data", n_booleanValue)
					aliases.put(id+"_effective", GAliasReference(id+"_active") & n_booleanValue)
				case _ =>
					if (node.flavor == NoneFlavor)
						throw new Exception("Interace has none flavor")
					val n_data = getImpl(id).foldLeft[GExpression](GIntLiteral(0))((c, id) => 
						c + GConditional(GAliasReference(id+"_effective"), GIntLiteral(1), GIntLiteral(0)))
					aliases.put(id+"_data", n_data)
					aliases.put(id+"_effective", if (node.flavor == BoolDataFlavor) 
						GAliasReference(id+"_active") & n_booleanValue else GAliasReference(id+"_active"))
			}
			IF[CompilationOptions.TRANSFORM_INTERFACE#v] {
				//create n_orGroup, n_allNegativeGroup, n_twoPositiveGroup, n_xorGroup
				val n_allNegativeGroup:GExpression = getImpl(id).foldLeft[GExpression](GBoolLiteral(true))((c:GExpression,id:String)=> c & GNot(GAliasReference(id+"_effective")))
				val n_xorGroup:GExpression = 
					(for (i <- 1 to getImpl(id).size) yield {
						var j = 0;
						(for (impl <- getImpl(id)) yield {
							j = j + 1
							if (i == j) GAliasReference(impl + "_effective")
							else GNot(GAliasReference(impl + "_effective"))
						}).foldLeft(GBoolLiteral(true):GExpression)((c, expr)=> c & expr)
						
					}).foldLeft(GBoolLiteral(false):GExpression)((c, expr)=> c | expr)
				val n_twoPositiveGroup = 
					GNot(n_xorGroup) & GNot(n_allNegativeGroup)
				aliases.put(id+"_orGroup", n_booleanValue)
				aliases.put(id+"_allNegativeGroup", n_allNegativeGroup)
				aliases.put(id+"_twoPositiveGroup", n_twoPositiveGroup)
				aliases.put(id+"_xorGroup", n_xorGroup)
			}
		}

		// convert requires
		var constraints = List[GExpression]()
		for(node <- allNodes; 
			req <- node.reqs) {
			val constraint = (!GAliasReference(node.id+"_effective")) | boolCDL2GExpression(req)
			setType(constraint, BoolType)
			constraints = constraint::constraints
		}
		
		//create equations for defaults for type inference
		var defaults = mutable.ListBuffer[GExpression]()
		for(node <- allNodes if node.defaultValue.isDefined) {
			val Some(default) = node.defaultValue
			defaults += CDL2GExpression(default) === GAliasReference(node.id + "_data") 
		}

		

		//convert legal values
		IF[CompilationOptions.CREATE_LEGAL_VALUES_CONSTRAINTS#v]{
			for(node <- allNodes if node.legalValues != None) {
				if (node.flavor != BoolDataFlavor && node.flavor != DataFlavor)
					throw new Exception("Legal values defined on none or bool:" + node)
				val Some(LegalValuesOption(ranges)) = node.legalValues
				val constraint = ranges.map( _ match {
						case MinMaxRange(low, high) => (CDL2GExpression(low) <= createRefData(node.id)) & (createRefData(node.id) <= CDL2GExpression(high))
						case SingleValueRange(v) => CDL2GExpression(v) === createRefData(node.id)
					}).reduceLeft(_ | _)
				setType(constraint, BoolType)
				constraints = ((!GAliasReference(node.id+"_effective")) | constraint)::constraints
			}
		}
/*
		//convert legal values
		IF[CompilationOptions.CREATE_LEGAL_VALUES_CONSTRAINTS#v]{
			for(node <- allNodes if node.legalValues != None) {
				if (node.flavor != BoolDataFlavor && node.flavor != DataFlavor)
					throw new Exception("Legal values defined on none or bool:" + node)
				val Some(LegalValuesOption(ranges)) = node.legalValues
                                val isEnumerationVal = isEnumeration(node)
                                if (!isEnumerationVal) {
                                  val constraint = ranges.map( _ match {
                                                  case MinMaxRange(low, high) => (CDL2GExpression(low) <= createRefData(node.id)) & (createRefData(node.id) <= CDL2GExpression(high))
                                                  case SingleValueRange(v) => {
//                                                     treat enumerations differently
//                                                    if (isEnumerationVal)
//                                                      CDL2GEnumExpression(v) === createRefData(node.id)
//                                                    else
                                                      CDL2GExpression(v) === createRefData(node.id)
                                                  }
                                          }).reduceLeft(_ | _)
//                                  println(constraint)
                                  setType(constraint, BoolType)
                                  constraints = ((!GAliasReference(node.id+"_effective")) | constraint)::constraints
                                } else {
                                  constraints = (!GAliasReference(node.id+"_effective"))::constraints
                                }
				
			}
		}
*/		
		def replaceSingle(t:GExpression, replacedIds:Set[String]):(GExpression,Set[String]) = t match {
			case e@GAliasReference(id) => 
				if (replacedIds.contains(id)) {
					errors += new CycleError(replacedIds)
					return (new GLiteral(1), replacedIds)
				}
				aliases.get(id) match {
					case Some(expr) =>
						replaceSingle(expr, replacedIds + id)
					case None => 
						errors += new NonExistingIdsError(id)
						(new GLiteral(0), replacedIds)
				}
			case x@_ => (x, replacedIds)
		}
		
		def replace(t:GExpression, replacedIds:Set[String]):GExpression = {
			val (t1, replacedIds1) = replaceSingle(t, replacedIds)
			if (t1.children.size == 0) 
				t1
			else {
				val numchildren = t1.asInstanceOf[Product].productArity
				val children = new Array[AnyRef](numchildren)
				for (i <- 0 until numchildren) {
					val ct = t1.asInstanceOf[Product].productElement (i)
					if (ct.isInstanceOf[GExpression]) {
						val ct1 = replace(ct.asInstanceOf[GExpression], replacedIds1)
						children(i) = ct1
					}
					else
						children(i) = ct.asInstanceOf[AnyRef]
				}
				val ctor = (t1.getClass.getConstructors())(0)
				try {
					val result = ctor.newInstance (children : _*).asInstanceOf[GExpression]
					// result.setExpectedType(t.expectedType)
					result
				} catch {
					case e : java.lang.ClassCastException =>
						error ("dup cast failed: " + t)
					case e : IllegalArgumentException =>
						error ("dup illegal arguments: " + ctor + " (" +
							   children.deep.mkString (",") + "), expects " +
							   ctor.getParameterTypes.length)
				}
			}
		}

		output("type inferencing...")
		for((id,expr)<-aliases) TypeGraph.getType(expr)
		constraints.foreach(TypeGraph.getType(_))
		defaults.foreach(TypeGraph.getType(_))
		IF[(CompilationOptions.TRACE_TYPE_PROPAGATION)#v] {
			println("\n***** initial Types *******")
			println(initialTypes)
		}
		for((expr,t)<-initialTypes) errors ++= TypeGraph.checkAndSetType(expr, t)
		errors ++= TypeGraph.propagateType()
		
		//remove optionals
		def removeOptionalSingle(t:Any):Any = t match {
			case x@GOptionalBoolFunc(e) if !(TypeGraph.getType(x) >= TypeGraph.getType(e)) && BoolType <= TypeGraph.getType(x) => GBoolFunc(e)
			case x@GOptionalBoolFunc(e) => e
			case x@_ => x
		}
		val removeOptional:GExpression=>GExpression = rewrite(geverywheretd(rulef(removeOptionalSingle)))(_)
		aliases = mutable.Map() ++ aliases.mapValues(removeOptional(_))
		constraints = constraints.map(removeOptional(_))
		
		//store types
		val storeTypeSingle:PartialFunction[Any, Unit] = {
			case x:TypeStored => {x.setType(TypeGraph.getType(x))}
		}
		def storeType(expr:GExpression):GExpression = 
			{rewrite(geverywheretd(query(storeTypeSingle)))(expr)}
		aliases.foreach(pair => storeType(pair._2))
		constraints.foreach(c => storeType(c))
		// aliases = mutable.Map() ++ aliases.mapValues(storeType)
		// constraints = constraints.map(storeType)
		assert(constraints.forall(_.getType() == BoolType), {val c = constraints.find(_.getType() != BoolType).get;c.toString + ":" + c.getType})
		
		output("total errors:" + errors.size + "\n")
		
		// Transform interface into propositional
		IF[(CompilationOptions.TRANSFORM_INTERFACE)#v] {
			def transformSingleInterface(t:Any):Any = {
				def featureID(id:String) = id.slice(0, id.size - 8)
				if (!t.isInstanceOf[GExpression]) return t
				val expr = t.asInstanceOf[GExpression]
				if ((expr::expr.children).foldLeft(false)((result, child)=>result || (child match {
					case GAliasReference(id) if id.endsWith("_refData") &&  allNodesMap.get(featureID(id)).map(_.cdlType).getOrElse(OptionType) == InterfaceType => true
					case _ => false
				}))) {
					IF[(CompilationOptions.PRINT_INTERFACE_USAGE)#v] {
						println(expr)
					}
					expr match {
						case GEq(GAliasReference(i), GIntValue(1)) => GAliasReference(featureID(i) + "_xorGroup")
						case GEq(GIntValue(1), GAliasReference(i)) => GAliasReference(featureID(i) + "_xorGroup")
						case GEq(GAliasReference(i), GIntValue(0)) => GAliasReference(featureID(i) + "_allNegativeGroup")
						case GEq(GIntValue(0), GAliasReference(i)) => GAliasReference(featureID(i) + "_allNegativeGroup")
						case GLessThan(GAliasReference(i), GIntValue(1)) => GAliasReference(featureID(i) + "_allNegativeGroup")
						case GGreaterThan(GIntValue(1), GAliasReference(i)) => GAliasReference(featureID(i) + "_allNegativeGroup")
						case GGreaterEqThan(GIntValue(1), GAliasReference(i)) => GAliasReference(featureID(i) + "_allNegativeGroup") | GAliasReference(featureID(i) + "_xorGroup")
						case GLessEqThan(GAliasReference(i), GIntValue(1)) => GAliasReference(featureID(i) + "_allNegativeGroup") | GAliasReference(featureID(i) + "_xorGroup")
						case GGreaterThan(GAliasReference(i), GIntValue(0)) => GAliasReference(featureID(i) + "_orGroup")
						case GGreaterEqThan(GAliasReference(i), GIntValue(1)) => GAliasReference(featureID(i) + "_orGroup")
						case GLessThan(GIntValue(0), GAliasReference(i)) => GAliasReference(featureID(i) + "_orGroup")
						case GBoolFunc(GAliasReference(i)) => GAliasReference(featureID(i) + "_orGroup")
						case GGreaterThan(GAliasReference(i), GIntValue(1)) => GAliasReference(featureID(i) + "_twoPositiveGroup")
						case GLessThan(GIntValue(1), GAliasReference(i)) => GAliasReference(featureID(i) + "_twoPositiveGroup")
						case _ => expr
					}
				}
				else expr
			}
			val transformInterface:GExpression=>GExpression =
				rewrite(geverywheretd(rulef(transformSingleInterface))) _

			aliases = mutable.Map() ++ aliases.mapValues(transformInterface)
			constraints = constraints.map(transformInterface)
		}

		output("calculating unsatisfiable constraints ... ")
		var unsatConstraints = mutable.Set[GExpression]()
		for		(constr <- constraints) {
			val result = GExpressionHelper.simplify(replace(constr, Set()))
			if (result.isInstanceOf[GBoolLiteral] && result.asInstanceOf[GBoolLiteral].value == false) {
				unsatConstraints += constr
				errors += new UnsatisfiableConstraintError(constr)
//				throw new Exception("Unsatisfiable constraint: " + constr)
			}
		}
		output("total unsat constraints: " + unsatConstraints.size)
		output("\n")
		
		output("replacing alias...")
		aliases = mutable.Map() ++ aliases.mapValues(replace(_, Set()))
		constraints = constraints.map(replace(_, Set())) 		

		// Remove conditional
		IF[CompilationOptions.REMOVE_CONDITIONAL#v] {
			output("removing conditionals...\n")
			constraints = constraints.map(GExpressionHelper.removeConditional)
		}
		
		// Remove boolean equal
		IF[CompilationOptions.REMOVE_CONDITIONAL#v] {
			output("removing boolean equal...\n")
			constraints = constraints.map(GExpressionHelper.removeBooleanEq)
		}

		output("simplifying... \n")

		constraints = constraints.map(GExpressionHelper.simplify(_))
		constraints = constraints.map(GExpressionHelper.removeGLiteral(_))

		output("calculating dead features ... ")
		
		var deadFeatures = mutable.Map[String, GExpression]()
		deadFeatures = mutable.Map() ++ aliases.mapValues(GExpressionHelper.simplify(_))
		deadFeatures = mutable.Map() ++ deadFeatures.mapValues(GExpressionHelper.removeGLiteral(_))
		deadFeatures = mutable.Map() ++ deadFeatures.filter(keyValue => {keyValue._1.contains("_active") && (keyValue._2.isInstanceOf[GBoolLiteral] && keyValue._2.asInstanceOf[GBoolLiteral].value == false) })

		deadFeatures.keySet.map(_.replaceAll("_active", "")).foreach(errors += DeadFeatureError(_))

		output("total dead features: " + deadFeatures.size + "\n")

//		var deadFeatureNames = mutable.Set[String]()
//		deadFeatureNames = mutable.Set() ++ deadFeatures.keySet
//		deadFeatureNames = mutable.Set() ++ deadFeatureNames.map(_.replaceAll("_active", ""))
		
		output("total errors:" + errors.size + "\n")

		var activeConstraints = mutable.Map[String, GExpression]()
		activeConstraints = mutable.Map() ++ aliases.mapValues(GExpressionHelper.simplify(_))
		activeConstraints = mutable.Map() ++ activeConstraints.mapValues(GExpressionHelper.removeGLiteral(_))
		activeConstraints = mutable.Map() ++ activeConstraints.filter(keyValue => keyValue._1.contains("_active"))

		var effectiveConstraints = mutable.Map[String, GExpression]()
		effectiveConstraints = mutable.Map() ++ aliases.mapValues(GExpressionHelper.simplify(_))
		effectiveConstraints = mutable.Map() ++ effectiveConstraints.mapValues(GExpressionHelper.removeGLiteral(_))
		effectiveConstraints = mutable.Map() ++ effectiveConstraints.filter(keyValue => keyValue._1.contains("_effective"))
		
		(variables, constraints, activeConstraints, effectiveConstraints, errors.toList)
	}
	
	// def analyzeFixes(vars, constraints) {
		// def getNumberOfFixes(constr) = {
			// ...
		// }
		// val fixNumbers:Seq[Int] = constraints.map(getNumberOfFixes)
		// val sum:Int = constraints.reduceLeft(_+_)
		// val average:Int = sum / constraints.size
		// val max:Int = constraints.reduceLeft((a, b)=>if (a > b) a b)
	// }
	
	
}
