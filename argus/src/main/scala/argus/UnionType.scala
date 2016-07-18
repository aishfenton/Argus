package argus

/**
  * Much magic. Very Lambda. Wow!
  *
  * These types provide union types (i.e. val a = (String or Int) within Scala. See here for good discussion on
  * how to represent union types in Scala
  *   http://stackoverflow.com/a/37450446/1591351
  *
  * Core idea is to adapt Demorgan's Law:
  *   $$ \lnot(A \cup B) \Leftrightarrow (\lnot A \cap \lnot B) $$
  *
  * To type Algerbra, which becomes:
  *   trait inv[-A]
  *   inv[A or B] :> (inv[A] with inv[B])
  *
  * Where we also use a contra-variant type, inv[-A], to give us a pseudo negation.
  *
  * The rest is then there to make the recursion work, so that when we write (A or B or C), which becomes Or[Or[A,B],C],
  * it still type checks.
  */
object UnionType {

  trait inv[-A] {}

//  type Or2[A, B] = {
//    type check[X] = (inv[A] with inv[B]) <:< inv[X]
//
//    // Since Or is non-exclusive, this is the intersection of A and B. Useful if you have types that aren't known until
//    // runtime. Best we can do is temporary cast them to be the most narrow type possible, so type constraints are meet,
//    // and then finally erase the type (i.e. Root[_]) so the user is forced to pattern match for which of A or
//    // B this type specifically is. Is there a better solution??
//    type intersect = A with B
//  }

  // Much magic. Very lambda. Wow.
  sealed trait OrR {
    type L <: OrR
    type R
    type invIntersect
    type intersect
  }

  sealed class Or[A <: OrR, B] extends OrR {
    type L = A
    type R = B

    type intersect = (L#intersect with R)
    type invIntersect = (L#invIntersect with inv[R])
    type check[X] = invIntersect <:< inv[X]
  }

  object UNil extends OrR {
    type intersect = Any
    type invIntersect = inv[Nothing]
  }
  type UNil = UNil.type

}
