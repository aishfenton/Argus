package argus.gen

import argus.schema.next.RefSchema

object RefMaker extends MakeFrom[RefSchema] {

  def apply(el: RefSchema) = {
    // find type referenced, and point to it
    val typ = ???
    Result(None, typ)
  }

}
