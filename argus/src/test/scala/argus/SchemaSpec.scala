package argus

import org.scalatest._
import pprint.Config.Colors._
import pprint.{PPrint, PPrinter}

class SchemaSpec extends FlatSpec with Matchers {

  // PPrint magic to make Options not print
//  implicit def ppOption[T: PPrint]: PPrinter[Option[T]] = PPrinter { (t: Option[T], c) => t match {
//    case Some(v) => implicitly[PPrint[T]].pprinter.render(v, c)
//    case _ => Iterator()
//  }}

  val simpleSchema = """
     | {
     |	"title": "Example Schema",
     |	"type": "object",
     |	"properties": {
     |		"firstName": {
     |			"type": "object"
     |		},
     |		"lastName": {
     |			"type": ["string", "array"]
     |		},
     |		"age": {
     |			"description": "Age in years",
     |			"type": "integer",
     |			"minimum": 0
     |		},
     |   "country": {
     |     "enum" : ["USA", "UK", "NZ"]
     |   }
     |	},
     |	"required": ["firstName", "lastName"]
     |}
   """.stripMargin

  "Schema" should "parse a simple Json schema" in {
    val schema = Schema.parse(simpleSchema)
    schema shouldBe a [Schema.Root[_]]
  }

  it should "" in {

  }

}
