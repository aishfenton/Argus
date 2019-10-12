package argus.gen

import argus.schema.next._
import scala.meta._
import scala.meta.tokens.Token.Ident

/**
  * Makes a Scala type for the given Schema. A `type` here means a) the definition statements that define that type
  * (i.e. case classes, enum objects, etc), and the type's ident token.
  *
  * NB: As schemas can be recursive, the returned definitions might also contain definitions of nested types.
  *
  * @tparam T The type of Schema
  */
trait MakeFrom[T <: Schema] {
  type Package = List[String]

  case class Result(stats: Option[Tree], typ: Ident)

  /**
    * Makes !
    * @param el The schema element to make from
    * @return The resulting definition statements and type
    */
  def apply(el: T): Result
}

object MakeFrom {

  trait Implicits {

    implicit class MakeFromOps[T <: Schema : MakeFrom](schema: T) {
      def make = implicitly[MakeFrom[T]].apply(schema)
    }

    implicit val refMaker = RefMaker
    implicit val objectMaker = ObjectMaker

  }

  object Implicits extends Implicits

}


