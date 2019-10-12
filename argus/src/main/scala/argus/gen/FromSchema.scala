package argus.gen

import argus.schema.next._

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.meta._
import scala.meta.contrib._

/**
  * Augments the annotated object with cases classes that implement the given Json Schema
  * @param json A string containing a Json schema
  * @param debug Dumps the generated code to stdout. Useful for debugging.
  * @param jsonEng The Json engine that we generate encode/decoders for. At the moment the only valid value is
  *                Some("Circe") or None
  * @param outPath Optional path, that if specified writes the generated code to a file at that path (defaults to None,
  *                so no file is written).
  * @param outPathPackage Optional package name, that if specified and if outPath also specified writes the package name
  *                       to the output file (defaults to None, so no package name is written).
  * @param name The name used for the root case class that is generated. Defaults to "Root"
  */
@compileTimeOnly("You must enable the macro paradise plugin.")
class fromSchemaJson(json: String, debug: Boolean = false, jsonEng: Option[String] = None, outPath: Option[String] = None,
                     outPathPackage: Option[String] = None, name: String = "Root") extends StaticAnnotation

/**
  * Same as fromSchemaJson, but loads the json schema from the given resource path
  * @param path Path to schema file within resources
  * @param debug Dumps the generated code to stdout. Useful for debugging.
  * @param jsonEng The Json engine that we generate encode/decoders for. At the moment the only valid value is
  *                Some("Circe") or None
  * @param outPath Optional path, that if specified writes the generated code to a file at that path (defaults to None,
  *                so no file is written).
  * @param outPathPackage Optional package name, that if specified and if outPath also specified writes the package name
  *                       to the output file (defaults to None, so no package name is written).
  * @param name The name used for the root case class that is generated. Defaults to "Root"
  */
@compileTimeOnly("You must enable the macro paradise plugin.")
class fromSchemaResource(path: String, debug: Boolean = false, jsonEng: Option[String] = None, outPath: Option[String] = None,
                         outPathPackage: Option[String] = None, name: String = "Root") extends StaticAnnotation

/**
  * Same as fromSchemaJson, but loads the json schema from the given file path.
  * @param url URL string to resource containing the json schema
  * @param debug Dumps the generated code to stdout. Useful for debugging.
  * @param jsonEng The Json engine that we generate encode/decoders for. At the moment the only valid value is
  *                Some("Circe") or None
  * @param outPath Optional path, that if specified writes the generated code to a file at that path (defaults to None,
  *                so no file is written).
  * @param outPathPackage Optional package name, that if specified and if outPath also specified writes the package name
  *                       to the output file (defaults to None, so no package name is written).
  * @param name The name used for the root case class that is generated. Defaults to "Root"
  */
@compileTimeOnly("You must enable the macro paradise plugin.")
class fromSchemaURL(url: String, debug: Boolean = false, jsonEng: Option[String] = None, outPath: Option[String],
                    outPathPackage: Option[String] = None, name: String = "Root") extends StaticAnnotation


object TransformOnce extends Transformer {

  override def apply(tree: Tree): Tree = tree match {
    case name @ Term.Name("b") => q"function($name)"
    case node => super.apply(node)
  }
}


trait Generator

class SchemaGenerator extends Generator {

//  private def findGenerators[B <: Tree: ModExtractor](a: B): List[Generator] = {
//    a.extract[Mod].collect {
//      case Mod.Annot(Init(Type.Name(n), _, _)) if generator_cache.contains(n) =>
//        generator_cache(n) match {
//          case m: ManipulationGenerator => m
//          case e: ExtensionGenerator => e
//          case t: TransmutationGenerator => t
//          case c: CompanionGenerator => c
//          case p: ParameterGenerator => p
//          case g =>
//            throw new IllegalStateException(
//              s"The runner cannot handle this type of generator: ${g.getClass.getSimpleName}")
//        }
//    }
//  }


  //  private val modelBuilder = new ModelBuilder[c.universe.type](c.universe)
//  private val codecBuilder = new CirceCodecBuilder[c.universe.type](c.universe)
//  private val helpers = new ASTHelpers[c.universe.type](c.universe)

  case class Params(schema: Schema,
                    debug: Boolean,
                    jsonEnd: Option[String],
                    outPath: Option[String],
                    outPathPackage: Option[String],
                    name: String)

  private def getParams(annot: Mod.Annot): Params = {
    val params = annot.extract[AnnotParams].head

    val schema = annot.init.name.value match {
//      case "fromSchemaJson" => {
//        Schema.fromJson(params.getAs[String](0, "json").get)
//      }
//      case "fromSchemaResource" => {
//        Schema.fromResource(params.getAs[String](0, "path").get)
//      }
//      case "fromSchemaURL" => {
//        Schema.fromURL(params.getAs[String](0, "url").get)
//      }
      case _ => throw new Exception("need to specify Schema")
    }

    Params(
      schema,
      params.getAs[Boolean](1, "debug").getOrElse(false),
      params.getAs[Option[String]](2, "jsonEng").getOrElse(None),
      params.getAs[Option[String]](3, "outPath").getOrElse(None),
      params.getAs[Option[String]](4, "outPathPackage").getOrElse(None),
      params.getAs[String](5, "name").getOrElse("Root")
    )
  }

//  private def saveToFile(path: String, packageName: Option[String], tree: Tree) = {
//    val packageCode = packageName.map(pn => s"package $pn;\n\n").getOrElse("")
//
//    // Clean up the code a little to make it more readable
//    val code = packageCode + showCode(tree)
//      .replaceAll(",", ",\n")
//      .replaceAll("\\.flatMap", "\n.flatMap")
//
//    val file = new java.io.File(path)
//    println("Writing generated code to: " + file.getAbsolutePath)
//
//    val writer = new java.io.PrintWriter(file)
//    try writer.write(code)
//    finally writer.close()
//  }

//  private def mkCodecs(jsonEng: Option[JsonEng], defs: List[Tree], path: List[String]): List[Tree] = {
//    val codecDefs = jsonEng match {
//      case Some(JsonEngs.Circe) => codecBuilder.mkCodec(defs, path)
//      case None => Nil
//      case a@_ => throw new Exception("Don't know JsonEng " + a)
//    }
//
//    if (codecDefs.isEmpty)
//      Nil
//    else
//      q"trait LowPriorityImplicits { ..$codecDefs }" ::
//        q"object Implicits extends LowPriorityImplicits" ::
//        Nil
//  }
//
//  private def mkModels = modelBuilder.mkSchemaDef(params.name, schema)
//
  def generate(annottee: Defn.Object): Unit = {

    val annot = annottee.extract[Mod.Annot].head
    val params = getParams(annot)
    val schema = params.schema

    val headerDefs =
      q"""
      class enum extends scala.annotation.StaticAnnotation
      class union extends scala.annotation.StaticAnnotation
      """

    val defs: List[Stat] = Nil // modelBuilder.mkSchemaDef(params.name)
//    val codecs = modelBuilder.mkSchemaDef(params.name)




  }



}

