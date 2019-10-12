package argus.gen

import argus.schema.next.{NamedSchema, ObjectSchema}

object ObjectMaker extends MakeFrom[ObjectSchema] {

  import MakeFrom.Implicits._

  def mkValDef(field: NamedSchema, isOptional: Boolean) = {
//    val (defs, typ) = field.schema.make
  }

  def apply(el: ObjectSchema) = {

    // Required
    val isOptional = el.required.getOrElse(Nil)

    // Build fields
    val fields = el.properties.map { p =>

    }

    // Build case class

    Result(Some(???), ???)
  }

}

