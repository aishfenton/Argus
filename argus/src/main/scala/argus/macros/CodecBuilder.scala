package argus.macros

import scala.reflect.api.Universe

/**
  * @author Aish Fenton.
  */
trait CodecBuilder {

  val u: Universe

  import u._

  val helpers = new ASTHelpers[u.type](u)

  import helpers._

  /**
    * Imports to place at start of generated code (i.e. io.circe._ )
    */
  def imports: List[Tree]

  /**
    * Constant definitions
    */
  def constants: List[Tree]

  /**
    * Makes an encoder for a given case class definition.
    * @param typ    The type of the case class (including path)
    * @param fields The case class's fields
    */
  def mkEncoder(typ: Tree, fields: List[Tree]): Tree

  /**
    * Makes an decoder for a given case class definition.
    * @param typ    The type of the case class (including path)
    * @param fields The case class's fields
    */
  def mkDecoder(typ: Tree, fields: List[Tree]): Tree

  def inEncoder(typ: Tree): Tree

  def inDecoder(typ: Tree): Tree

  /**
    * Makes an encoder for the an any wrappers
    * @param typ  The type of the wrapper
    */
  def mkAnyWrapperEncoder(typ: Tree): Tree

  /**
    * Makes a decoder for the an any wrapper
    * @param typ  The type of the wrapper
    */
  def mkAnyWrapperDecoder(typ: Tree): Tree

  /**
    * Makes encoder instance definition for encoding enum types (without assigning it to a val)
    *
    * @param typ          The super type that is used to group all the enums
    * @param subTermPairs A list of tuples containing the jsonStr (which encodes the value of the enum) and the
    *                     TermName (which encodes the singleton of the enum).
    */
  def mkEnumEncoder(typ: Tree, subTermPairs: List[(String, Tree)]): Tree

  /**
    * Makes decoder instance definition for decoding enum types (without assigning it to a val)
    *
    * @param typ          The super type that is used to group all the enums
    * @param subTermPairs A list of tuples containing the jsonStr (which encodes the value of the enum) and the
    *                     TermName (which encodes the singleton of the enum).
    */
  def mkEnumDecoder(typ: Tree, subTermPairs: List[(String, Tree)]): Tree

  /**
    * Makes encoder instance definition for encoding union types (without assigning it to a val)
    *
    * @param typ      The super type that is used to group all the enums
    * @param subTypes A list of tuples containing the rawType (i.e. Int) and the union wrapper type (i.e. FooInt)
    */
  def mkUnionEncoder(typ: Tree, subTypes: List[(Tree, Tree)]): Tree

  /**
    * Makes decoder instance definition for decoding union types (without assigning it to a val)
    *
    * @param typ      The super type that is used to group all the enums
    * @param subTypes A list of tuples containing the rawType (i.e. Int) and the union wrapper type (i.e. FooInt)
    */
  def mkUnionDecoder(typ: Tree, subTypes: List[(Tree, Tree)]): Tree


  def mkEncoderValDef(typ: Tree, encDef: Tree): Tree = {
    val name = TermName(nameFromType(typ) + "Encoder")
    q"implicit val $name: ${inEncoder(typ)} = $encDef"
  }

  def mkDecoderValDef(typ: Tree, decDef: Tree): Tree = {
    val name = TermName(nameFromType(typ) + "Decoder")
    q"implicit val $name: ${inDecoder(typ)} = $decDef"
  }

  def isAnyParam(params: List[ValDef]) = params.length == 1 &&
    params.head.equalsStructure(q"${Modifiers(Flag.CASEACCESSOR | Flag.PARAMACCESSOR)} val x: Any")

  /**
    * Gets all the trait def types that are annotated as being a union
    */
  def collectUnionTypes(defs: List[Tree]) = defs collect {
    case (q"$mods trait $name extends ..$_")
      if hasAnnotation(mods, "union") => Ident(name)
  }

  def mkCodec(defs: List[Tree], path: List[String] = Nil): List[Tree] = {

    def mkCodecRec(defs: List[Tree], path: List[String], unionTypes: List[Tree]): List[Tree] = defs.collect {

      //----
      // Enums
      //----
      case (q"$mods trait $tname extends ..$_ { ..$_ }")
        if mods.hasFlag(Flag.SEALED) && mods.annotations.exists(_.equalsStructure(q"new enum()")) => {

        // Extract details of case object
        val members = collectExtendsType(path, Ident(tname), defs).map { case(path, subDef) =>
          val q"case object $name extends $_ { val json: String = $jsonLit }" = subDef
          val Literal(Constant(jsonStr: String)) = jsonLit
          (jsonStr, mkSelectPath(path :+ name.toString))
        }

        val fullTyp = mkTypeSelectPath(path :+ tname.toString)
        val encDef = mkEnumEncoder(fullTyp, members)
        val decDef = mkEnumDecoder(fullTyp, members)
        mkEncoderValDef(fullTyp, encDef) :: mkDecoderValDef(fullTyp, decDef) :: Nil
      }

      //----
      // Union types
      //----
      case (q"$mods trait $tname extends ..$_")
        if mods.hasFlag(Flag.SEALED) && mods.annotations.exists(_.equalsStructure(q"new union()")) => {

        // Extract details of case object
        val members = collectExtendsType(path, Ident(tname), defs).map { case(path, subDef) =>
          val q"case class $unionType(x: $rawType) extends $_" = subDef
          (rawType, mkTypeSelectPath(path :+ unionType.toString))
        }

        val fullTyp = mkTypeSelectPath(path :+ tname.toString)
        val encDef = mkUnionEncoder(fullTyp, members)
        val decDef = mkUnionDecoder(fullTyp, members)
        mkEncoderValDef(fullTyp, encDef) :: mkDecoderValDef(fullTyp, decDef) :: Nil
      }

      //----
      // Any wrappers
      //----
      case (q"$mods class $tname(..$params) extends ..$_")
        if mods.hasFlag(Flag.CASE) && isAnyParam(params) => {

        val fullTyp = mkTypeSelectPath(path :+ tname.toString)
        val encDef = mkAnyWrapperEncoder(fullTyp)
        val decDef = mkAnyWrapperDecoder(fullTyp)

        mkEncoderValDef(fullTyp, encDef) :: mkDecoderValDef(fullTyp, decDef) :: Nil
      }

      //----
      // Case classes
      //----
      case (q"$mods class $tname(..$params) extends ..$parents")
        if mods.hasFlag(Flag.CASE) && treesIntersect(parents, unionTypes).isEmpty && !isAnyParam(params) => {

        val fullTyp = mkTypeSelectPath(path :+ tname.toString)
        val encDef = mkEncoder(fullTyp, params)
        val decDef = mkDecoder(fullTyp, params)

        mkEncoderValDef(fullTyp, encDef) :: mkDecoderValDef(fullTyp, decDef) :: Nil
      }

      // If object defined, then recurse into it
      case (q"$mods object $name { ..$innerDefs }") => {
        mkCodecRec(innerDefs, path :+ name.toString, collectUnionTypes(innerDefs))
      }

    }.flatten

    imports ++ constants ++ mkCodecRec(defs, path, collectUnionTypes(defs))
  }


}
