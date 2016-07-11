package argus

/**
  * Magic to provide support for union types (i.e. val a = (String or Int).
  * See here for explanation on why this works here:
  *   http://stackoverflow.com/a/37450446/1591351
  */
object UnionType {

  // Much magic. Very lambda. Wow.
  trait inv[-A] {}

  type Or[A, B] = {
    type check[X] = (inv[A] with inv[B]) <:< inv[X]

    // Since Or is non-exclusive, this is the intersection of A and B. Useful if you have types that aren't known until
    // runtime. Best we can do is temporary cast them to be the most narrow type possible, so type constraints are meet,
    // and then finally erase the type (i.e. Root[_]) so the user is forced to pattern match for which of A or
    // B this type specifically is. Is there a better solution??
    type intersect = A with B
  }

//  trait OrR {
//    type nextL
//    type R
//
//    type X[L <: OrR, R]
//  }
//
//  trait OrA extends OrR {
//    type nextL = OrA
//    type R = Int
//
//    type X[L <: OrR, R] = L#X[L#nextL, R]
//  }

  sealed trait OrR {
    type L <: OrR
    type R
    type invIntersect
    type intersect
  }

  sealed class OrA[A <: OrR, B] extends OrR {
    type L = A
    type R = B

    type intersect = (L#intersect with R)
    type invIntersect = (L#invIntersect with inv[R])
    type check[X] = invIntersect <:< inv[X]
  }

  object UNil extends OrR {
//    type L = Nothing
//    type R = Nothing
    type intersect = Any
    type invIntersect = inv[Nothing]
  }
  type UNil = UNil.type

//  sealed class OrNil[A, B] extends OrR {
//    type L = A
//    type R = B
//
//    type invIntersect = inv[A] with inv[B]
////    type check[X] = (inv[A] with inv[B]) <:< inv[X]
//  }

  type Or3[A, B, C] = {
    type check[X] = (inv[A] with inv[B] with inv[C]) <:< inv[X]
    type intersect = A with B with C
  }

//  sealed trait OrR[L, R] {
//    type check[X] // = (inv[L] with inv[R]) <:< inv[X]
//    type invIntersect = inv[L] with inv[R]
//    type intersect = L with R
//  }
//
//  sealed class OrA[L <: OrR[_,_], R] extends OrR[L, R] {
//    override type check[X] = (L#invIntersect with inv[R]) <:< inv[X]
//  }
//
//  sealed class OrNil[L, R] extends OrR[L, R] {
//    override type check[X] = (inv[L] with inv[R]) <:< inv[X]
//  }

//   def foo[X : ((A orNil B) orA C)#checked]

}
