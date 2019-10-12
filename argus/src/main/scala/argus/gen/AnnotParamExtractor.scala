package argus.gen

import scala.meta._
import scala.meta.contrib._

case class AnnotParam(value: Any, name: Option[String] = None)

case class AnnotParams(args: List[AnnotParam]) {

  /**
    * Gets a param by name or position. Name takes precedence over position.
    * @param pos The argument's position
    * @param name The arguments name
    * @tparam T The arguments types
    */
  def getAs[T](pos: Int, name: String): Option[T] = args
    .find(_.name == name)
    .orElse(Some(args(pos)))
    .map(_.value.asInstanceOf[T])


  def getAll = args.map(_.value)

}

class AnnotParamsExtractor extends Extract[Mod.Annot, AnnotParams] {

  /**
    * Extracts params from an annotation (Mod.Annot)". Arguments must be literal types, Some(v)/None of a literal type,
    * or a Seq of literal types. Named arguments as well as positional args are supported.
    *
    * @param annot The annotation to exract from
    * @return A list of AnnotParams (since list, since there can be multiple argument blocks)
    */
  def extract(annot: Mod.Annot): List[AnnotParams] = {
    annot.init.argss.map { t => AnnotParams(t.map(extractOne)) }
  }

  private def extractOne(param: Term): AnnotParam = {
    def matchValue(argV: Term): Any = argV match {

      // literals
      case Lit.Boolean(b) => b
      case Lit.Byte(b)    => b
      case Lit.Short(s)   => s
      case Lit.Int(i)     => i
      case Lit.Long(l)    => l

      // XXX these two return string, but I think it's a bug.
      // reported here: https://github.com/scalameta/scalameta/issues/1535
      case Lit.Float(f)   => f.toFloat
      case Lit.Double(d)  => d.toDouble

      case Lit.Char(c)    => c
      case Lit.String(s)  => s
      case Lit.Symbol(s)  => s

      // literals wrapped in Some|None
      case Term.Name("None") => None
      case Term.Apply(Term.Name("Some"), inner :: Nil) => Some(matchValue(inner))

      case Term.Apply(Term.Name("Seq"), values: List[Term]) => values.map(matchValue)
      case Term.Apply(Term.Name("List"), values: List[Term]) => values.map(matchValue)

      case _ => throw new Exception("Can't handle param. Is it a literal type? " + param.toString)
    }

    param match {
      // named
      case Term.Assign(Term.Name(name), v) => AnnotParam(matchValue(v), Some(name))

      // pos
      case _ => AnnotParam(matchValue(param), None)
    }
  }

}


