package gsd.cdl.formula

import org.kiama.rewriting.Rewriter._

object GRewriter {
	def geverywheretd (s : => Strategy) : Strategy =
		gtopdown (attempt (s)) 
		
    def geverywherebu (s : => Strategy) : Strategy =
        gbottomup (attempt (s)) 		

    def gbottomup (s : => Strategy) : Strategy =
        gall (gbottomup (s)) <* s 		
		
	def gtopdown (s : => Strategy) : Strategy =
		s <* gall (gtopdown (s)) 
		
	def gall (s : => Strategy) : Strategy =
		new Strategy {
			def apply (t : Term) : Option[Term] =
				t match {
					case p : GExpression => gallProduct (p, s)
					case a           => Some (a)
				}
		}

	private def gallProduct (p : GExpression, s : => Strategy) : Option[Term] = {
		val numchildren = p.asInstanceOf[Product].productArity
		if (numchildren == 0) {
			Some (p)
		} else {
			val children = new Array[AnyRef](numchildren)
			var hasGExpr = false
			for (i <- 0 until numchildren) {
				val ct = p.asInstanceOf[Product].productElement (i)
				if (ct.isInstanceOf[GExpression]) hasGExpr = true
				s (ct) match {
					case Some (ti) =>
						children (i) = makechild (ti)
					case None      =>
						return None
				}
			}
			if (hasGExpr) {
				val ret = gdup (p, children)
				Some (ret)
			}
			else Some (p)
		}
	} 	
	
	private def gdup (t : GExpression, children : Array[AnyRef]) : GExpression = {
        val ctor = (t.getClass.getConstructors())(0)
        try {
            val result = ctor.newInstance (children : _*).asInstanceOf[GExpression]
			if (result.isInstanceOf[TypeStored]) 	
				result.asInstanceOf[TypeStored].setType(t.asInstanceOf[TypeStored].getType())
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
	
	private def makechild (child : Any) : AnyRef = {
        try {
            return child.asInstanceOf[AnyRef]
        } catch {
            case e : ClassCastException =>
                error ("makechild: can't cast child: " + child + " " + e)
        }
    } 
}
