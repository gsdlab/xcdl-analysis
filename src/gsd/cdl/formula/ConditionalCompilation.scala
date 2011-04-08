package gsd.cdl.formula

object ConditionalCompilation {
  trait BOOL {
    type a[t <: BOOL, f <: BOOL] <: BOOL
    type v = a[TRUE, FALSE]
  }
  final class TRUE extends BOOL {
    type a[t <: BOOL, f <: BOOL] = t
  }
  final class FALSE extends BOOL{
    type a[t <: BOOL, f <: BOOL] = f
  }
  trait IF[x <: BOOL, y <: BOOL, z <: BOOL] extends BOOL {
    type a[t <: BOOL, f <: BOOL] = x#a[y, z]#a[t, f]
  }
  trait NOT[x <: BOOL] extends BOOL {
    type a[t <: BOOL, f <: BOOL] = IF[x, FALSE, TRUE]#a[t, f]
  }
  trait AND[x <: BOOL, y <: BOOL] extends BOOL {
    type a[t <: BOOL, f <: BOOL] = IF[x, y, FALSE]#a[t, f]
  }
  trait OR[x <: BOOL, y <: BOOL] extends BOOL {
    type a[t <: BOOL, f <: BOOL] = IF[x, TRUE, y]#a[t, f]
  }

  // aliases for nicer syntax
  type ![x <: BOOL] = NOT[x]
  type ||[x <: BOOL, y <: BOOL] = OR[x, y]
  type &&[x <: BOOL, y <: BOOL] = AND[x, y]
  
  def IF[B] = null.asInstanceOf[B]

  object Include {
    def apply(block: => Unit) {
      block
    }
  }

  object Exclude {
    def apply(block: => Unit) { }
  }

  implicit def include(t: TRUE) = Include
  implicit def exclude(f: FALSE) = Exclude

}