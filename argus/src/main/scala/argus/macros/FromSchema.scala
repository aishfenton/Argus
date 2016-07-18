package argus.macros

import java.io.File

import argus.schema._
import macrocompat.bundle

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

/**
  * Augments the annotated object with cases classes that implement the given Json Schema
  * @param json A string containing a Json schema
  */
@compileTimeOnly("You must enable the macro paradise plugin.")
class fromSchemaJson(json: String) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro SchemaMacros.fromSchemaMacroImpl
}

/**
  * Same as fromSchemaJson, but loads the json schema from the given resource path
  * @param path Path to schema file within resources
  */
@compileTimeOnly("You must enable the macro paradise plugin.")
class fromSchemaResource(path: String) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro SchemaMacros.fromSchemaMacroImpl
}

/**
  * Same as fromSchemaJson, but loads the json schema from the given file path.
  * @param url URL string to resource containing the json schema
  */
@compileTimeOnly("You must enable the macro paradise plugin.")
class fromSchemaURL(url: String) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro SchemaMacros.fromSchemaMacroImpl
}

@bundle
class SchemaMacros(val c: Context) {
  import c.universe._

  private val modelBuilder = new ModelBuilder[c.type](c)

  private def parse(path: String) = Schema.fromResource(path)

  private def readSchema(prefix: Tree) = prefix match {
    case q"new fromSchemaResource (..$params)" => params.map { case(Literal(Constant(x))) => Schema.fromResource(x.toString) }.head
    case q"new fromSchemaURL (..$params)" => params.map { case(Literal(Constant(x))) => Schema.fromURL(x.toString) }.head
    case q"new fromSchemaJson (..$params)" => params.map { case(Literal(Constant(x))) => Schema.fromJson(x.toString) }.head
    case _ => c.abort(c.enclosingPosition, "Annotation argument must be a string constant")
  }

  def fromSchemaMacroImpl(annottees: c.Expr[Any]*): c.Expr[Any] = {

    val schema = readSchema(c.prefix.tree)

    val defs = modelBuilder.mkSchema("Root", schema)

    val result = annottees map (_.tree) match {

      // Match object
      case (objDef @ q"$mods object $tname extends { ..$earlydefns } with ..$parents { $self => ..$stats }") :: _ => {
        q"""
          $mods object $tname extends { ..$earlydefns } with ..$parents { $self =>
            ..$defs

            ..$stats
          }
        """
      }

      case _ => c.abort(c.enclosingPosition, "Invalid annotation target: Needs to be an object !!")

    }

    println(showCode(result))
    c.Expr[Any](result)
  }

}



