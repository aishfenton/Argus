package argus.macros

import argus.schema._
import macrocompat.bundle

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

sealed trait JsonEng
object JsonEngs {
  case object Circe extends JsonEng
}

/**
  * Augments the annotated object with cases classes that implement the given Json Schema
  * @param json A string containing a Json schema
  * @param jsonEng The Json engine that we generate encode/decoders for. At the moment the only valid value is
  *                Some(JsonEngs.Circe) or None
  * @param debug Dumps the generated code to stdout. Useful for debugging.
  */
@compileTimeOnly("You must enable the macro paradise plugin.")
class fromSchemaJson(json: String, jsonEng: Option[JsonEng] = None, debug: Boolean = false) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro SchemaMacros.fromSchemaMacroImpl
}

/**
  * Same as fromSchemaJson, but loads the json schema from the given resource path
  * @param path Path to schema file within resources
  * @param debug Dumps the generated code to stdout. Useful for debugging.
  */
@compileTimeOnly("You must enable the macro paradise plugin.")
class fromSchemaResource(path: String, debug: Boolean = false) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro SchemaMacros.fromSchemaMacroImpl
}

/**
  * Same as fromSchemaJson, but loads the json schema from the given file path.
  * @param url URL string to resource containing the json schema
  * @param debug Dumps the generated code to stdout. Useful for debugging.
  */
@compileTimeOnly("You must enable the macro paradise plugin.")
class fromSchemaURL(url: String, debug: Boolean = false) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro SchemaMacros.fromSchemaMacroImpl
}

@bundle
class SchemaMacros(val c: Context) {
  import c.universe._

  private val modelBuilder = new ModelBuilder[c.universe.type](c.universe)
  private val codecBuilder = new CirceCodecBuilder[c.universe.type](c.universe)

  private def parse(path: String) = Schema.fromResource(path)

  private def params(prefix: Tree) = {
    val q"new $name (..$params)" = prefix

    val (Ident(TypeName(fn: String))) = name

    // Get params
    val content = (params lift) (0) map { case Literal(Constant(x: String)) => x } get
    val jsonEng = (params lift) (1) map {
      case q"Some(JsonEngs.$eng)" => Some(eng.toString)
      case q"None" => None
    } getOrElse(None)
    val debug = (params lift) (2) map { case Literal(Constant(debug: Boolean)) => debug } getOrElse(false)

    (fn, content, jsonEng, debug)
  }

  private def readSchema(fn: String, content: String) = fn match {
    case "fromSchemaResource" => Schema.fromResource(content)
    case "fromSchemaURL" => Schema.fromURL(content)
    case "fromSchemaJson" => Schema.fromJson(content)
    case _ => c.abort(c.enclosingPosition, "Didn't know annotation " + fn)
  }

  private def mkCodecs(jsonEng: Option[String], defs: List[Tree], path: List[String]) = {
    val codecDefs = jsonEng match {
      case Some("Circe") => codecBuilder.mkCodec(defs, path)
      case _ => Nil
    }
    if (codecDefs.isEmpty) EmptyTree else q"object Implicits { ..$codecDefs }"
  }

  def fromSchemaMacroImpl(annottees: c.Expr[Any]*): c.Expr[Any] = {
    val (fn, content, jsonEng, debug) = params(c.prefix.tree)
    val schema = readSchema(fn, content)

    val result = annottees map (_.tree) match {

      // Add definitions and codecs to annotated object
      case (objDef @ q"$mods object $tname extends { ..$earlydefns } with ..$parents { $self => ..$stats }") :: _ => {

        val path = List(tname.toString)
        val defs = modelBuilder.mkSchemaDef(path, "Root", schema)

        q"""
          $mods object $tname extends { ..$earlydefns } with ..$parents { $self =>
            ..$defs
            ..$stats
            ${ mkCodecs(jsonEng, defs, path) }
          }
        """
      }

      case _ => c.abort(c.enclosingPosition, "Invalid annotation target: Needs to be an object !!")
    }

    if (debug) println(showCode(result))
    c.Expr[Any](result)
  }

}



