package argus.macros

import argus.schema._

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
  * @param debug Dumps the generated code to stdout. Useful for debugging.
  * @param jsonEng The Json engine that we generate encode/decoders for. At the moment the only valid value is
  *                Some(JsonEngs.Circe) or None
  * @param outPath Optional path, that if specified writes the generated code to a file at that path (defaults to None,
  *                so no file is written).
  * @param outPathPackage Optional package name, that if specified and if outPath also specified writes the package name
  *                       to the output file (defaults to None, so no package name is written).
  * @param name The name used for the root case class that is generated. Defaults to "Root"
  */
@compileTimeOnly("You must enable the macro paradise plugin.")
class fromSchemaJson(json: String, debug: Boolean = false, jsonEng: Option[JsonEng] = None, outPath: Option[String] = None,
                     outPathPackage: Option[String] = None, name: String = "Root") extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro SchemaMacros.fromSchemaMacroImpl
}

/**
  * Same as fromSchemaJson, but loads the json schema from the given resource path
  * @param path Path to schema file within resources
  * @param debug Dumps the generated code to stdout. Useful for debugging.
  * @param jsonEng The Json engine that we generate encode/decoders for. At the moment the only valid value is
  *                Some(JsonEngs.Circe) or None
  * @param outPath Optional path, that if specified writes the generated code to a file at that path (defaults to None,
  *                so no file is written).
  * @param outPathPackage Optional package name, that if specified and if outPath also specified writes the package name
  *                       to the output file (defaults to None, so no package name is written).
  * @param name The name used for the root case class that is generated. Defaults to "Root"
  */
@compileTimeOnly("You must enable the macro paradise plugin.")
class fromSchemaResource(path: String, debug: Boolean = false, jsonEng: Option[JsonEng] = None, outPath: Option[String] = None,
                         outPathPackage: Option[String] = None, name: String = "Root") extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro SchemaMacros.fromSchemaMacroImpl
}

/**
  * Same as fromSchemaJson, but loads the json schema from the given file path.
  * @param url URL string to resource containing the json schema
  * @param debug Dumps the generated code to stdout. Useful for debugging.
  * @param jsonEng The Json engine that we generate encode/decoders for. At the moment the only valid value is
  *                Some(JsonEngs.Circe) or None
  * @param outPath Optional path, that if specified writes the generated code to a file at that path (defaults to None,
  *                so no file is written).
  * @param outPathPackage Optional package name, that if specified and if outPath also specified writes the package name
  *                       to the output file (defaults to None, so no package name is written).
  * @param name The name used for the root case class that is generated. Defaults to "Root"
  */
@compileTimeOnly("You must enable the macro paradise plugin.")
class fromSchemaURL(url: String, debug: Boolean = false, jsonEng: Option[JsonEng] = None, outPath: Option[String],
                    outPathPackage: Option[String] = None, name: String = "Root") extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro SchemaMacros.fromSchemaMacroImpl
}


class SchemaMacros(val c: Context) {
  import c.universe._

  private val modelBuilder = new ModelBuilder[c.universe.type](c.universe)
  private val codecBuilder = new CirceCodecBuilder[c.universe.type](c.universe)
  private val helpers = new ASTHelpers[c.universe.type](c.universe)
  import helpers._

  case class Params(schema: Schema.Root, debug: Boolean, jsonEnd: Option[JsonEng], outPath: Option[String],
                    outPathPackage: Option[String], name: String)

  private def extractParams(prefix: Tree): Params = {
    val q"new $name (..$paramASTs)" = prefix
    val (Ident(TypeName(fn: String))) = name

    val commonParams = ("debug", false) :: ("jsonEng", q"Some(JsonEngs.Circe)") :: ("outPath", None) ::
      ("outPathPackage", None) :: ("name", "Root") :: Nil

    val params = fn match {
      case "fromSchemaResource" => {
        val params = paramsToMap(("path", "Path missing") :: commonParams, paramASTs)
        params + ("schema" -> Schema.fromResource(params("path").asInstanceOf[String]))
      }
      case "fromSchemaURL" => {
        val params = paramsToMap(("url", "URL missing") :: commonParams, paramASTs)
        params + ("schema" -> Schema.fromURL(params("url").asInstanceOf[String]))
      }

      case "fromSchemaJson" => {
        val params = paramsToMap(("json", "Json missing") :: commonParams, paramASTs)
        params + ("schema" -> Schema.fromJson(params("json").asInstanceOf[String]))
      }

      case _ => c.abort(c.enclosingPosition, "Didn't know annotation " + fn)
    }

    Params(
      params("schema").asInstanceOf[Schema.Root],
      params("debug").asInstanceOf[Boolean],
      params("jsonEng") match { case q"Some(JsonEngs.Circe)" => Some(JsonEngs.Circe); case q"None" => None },
      params("outPath").asInstanceOf[Option[String]],
      params("outPathPackage").asInstanceOf[Option[String]],
      params("name").asInstanceOf[String]
    )
  }

  private def saveToFile(path: String, packageName: Option[String], tree: Tree) = {
    val packageCode = packageName.map(pn => s"package $pn;\n\n").getOrElse("")

    // Clean up the code a little to make it more readable
    val code = packageCode + showCode(tree)
      .replaceAll(",", ",\n")
      .replaceAll("\\.flatMap", "\n.flatMap")

    val file = new java.io.File(path)
    println("Writing generated code to: " + file.getAbsolutePath)

    val writer = new java.io.PrintWriter(file)
    try writer.write(code)
    finally writer.close()
  }

  private def mkCodecs(jsonEng: Option[JsonEng], defs: List[Tree], path: List[String]): List[Tree] = {
    val codecDefs = jsonEng match {
      case Some(JsonEngs.Circe) => codecBuilder.mkCodec(defs, path)
      case None => Nil
      case a@_ => throw new Exception("Don't know JsonEng " + a)
    }

    if (codecDefs.isEmpty)
      Nil
    else
      q"trait LowPriorityImplicits { ..$codecDefs }" ::
        q"object Implicits extends LowPriorityImplicits" ::
        Nil
  }

  def fromSchemaMacroImpl(annottees: c.Expr[Any]*): c.Expr[Any] = {
    val params = extractParams(c.prefix.tree)
    val schema = params.schema

    val result: Tree = annottees map (_.tree) match {

      // Add definitions and codecs to annotated object
      case (objDef @ q"$mods object $tname extends { ..$earlydefns } with ..$parents { $self => ..$stats }") :: _ => {

        val (_, defs) = modelBuilder.mkSchemaDef(params.name, schema)

        q"""
          $mods object $tname extends { ..$earlydefns } with ..$parents { $self =>
            ..$stats

            class enum extends scala.annotation.StaticAnnotation
            class union extends scala.annotation.StaticAnnotation

            ..$defs
            ..${ mkCodecs(params.jsonEnd, defs, tname.toString :: Nil) }
          }
        """
      }

      case _ => c.abort(c.enclosingPosition, "Invalid annotation target: Needs to be an object !!")
    }

    if (params.debug) println(showCode(result))

    params.outPath match { case Some(path) => saveToFile(path, params.outPathPackage, result); case None => /* noop */ }

    c.Expr[Any](result)
  }

}

