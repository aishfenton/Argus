package argus.gen

import scala.meta._
import scala.meta.contrib._

case class AnnotParam(value: Any, name: Option[String] = None)

class AnnotationParamExtractor extends Extract[Mod.Annot, AnnotParam] {

  /**
    * Extracts argument values from a List[Tree] as is obtained from a quasiquote such as q"myFunc(..$params))".
    * Supports named arguments, and default values.
    * @param nameAndDefaults A list of tuples containing each argument's name and default value. This must be the complete
    *                        list of supported arguments.
    * @param params A list of trees to as extracted by ..$params. If arguments are constants or option wrapped constants
    *               then their reified values are returned (i.e. Some(1)), if not then the AST chunk is returned.
    *
    * @return A map of argument names and their extracted values (or their default value if not extracted)
    */
  def extract(annot: Mod.Annot): List[AnnotParam] = {
    annot.init.argss.flatten.map(extractOne)
  }

  private def extractOne(param: Term): AnnotParam = {
    def matchValue(argV: Term): Any = argV match {

      // literals
      case Lit.Boolean(b) => b
      case Lit.Double(d)  => d
      case Lit.Int(i)     => i
      case Lit.String(s)  => s
      case Lit.Symbol(s)  => s

      // literals wrapped in Some|None
      case Term.Name("None") => None
      case Term.Apply(Term.Name("Some"), List(inner)) => Some(matchValue(inner))

      case _ => throw new Exception("Can't handle param: " + param.toString)
    }

    param match {
      // Named
      case Term.Assign(Term.Name(name), v) => AnnotParam(matchValue(v), Some(name))

      // pos
      case _ => AnnotParam(matchValue(param), None)
    }
  }

}
