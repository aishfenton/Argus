# Argus

Scala macros for generating code from [Json Schemas](http://json-schema.org). Everything defined in the
schema has case classes and json encoders/decoders generated for it.

## Quick Example

Starting with this Json schema.
```json
{
  "title": "Example Schema",
  "type": "object",
  "definitions" : {
    "Address": {
      "type": "object",
      "properties": {
        "number" : { "type": "integer" },
        "street" : { "type": "string" }
      }
    },
    "ErdosNumber": {
      "type": "integer"
    }
  },
  "properties": {
    "name": {
      "type": "array",
      "items": { "type": "string" }
    },
    "age": {
      "description": "Age in years",
      "type": "integer"
    },
    "address" : { "$ref" : "#/definitions/Address" },
    "erdosNumber" : { "$ref" : "#/definitions/ErdosNumber" }
  }
}
```

Many more examples [here](argus/src/test/scala/argus/macros/FromSchemaSpec.scala)

We can use the @fromSchemaResource macro to generate case classes for (Root, Address)

```scala
import argus.macros._
import io.circe._
import io.circe.syntax._

@fromSchemaResource("/simple.json", name="Person")
object Schema
import Schema._
import Schema.Implicits._

val json = """
   |{
   |  "name" : ["Bob", "Smith"],
   |  "age" : 26,
   |  "address" : {
   |    "number" : 31,
   |    "street" : "Main St"
   |  },
   |  "erdosNumber": 123
   |}
  """.stripMargin

// Decode into generated case class
val person = parser.decode[Person](json).toOption.get

// Update address 
val address = Address(number=Some(42), street=Some("Alt St"))
val newPerson = person.copy(address=Some(address))

// Encode base to json
newPerson.asJson 
```

**NB:**
Why Argus? In keeping with the theme of Argonaut and Circe, Argus (son of Arestor) was the builder of the ship "Argo", 
on which the Argonauts sailed.

# Rules

## Supported constructs

<table>
<tr><td>Json</td><td>Generated Scala</td>
<tr>
<td><pre>
{
  "properties" : { 
    "name" : { "type" : "string" },
    "age"  : { "type" : "integer" }
  },
  "required" : ["name"]
}
</pre></td>
<td><pre>
case class Root(name: String, age: Option[Int] = None)
</pre></td>
</tr>

<tr>
<td><pre>
{
  "definitions" : { 
    "Person" : { ... },
    ...
  }
  "properties" : { 
    "person" : { "$ref" : "#/definitions/Person" }
  }
}
</pre></td>
<td><pre>
case class Person(...)
case class Root(person: Option[Person] = None)
</pre></td>
</tr>

<tr>
<td><pre>
{
  "oneOf": [
    { "$ref" : "#/definitions/Address" },
    { "type" : "number" }
  ]
}
</pre></td>
<td><pre>
@union sealed trait RootUnion
case class RootAddress(...) extends RootUnion
case class RootDouble(...) extends RootUnion
</pre></td>
</tr>

</table>


## Unsupported

* *anyOf* / *allOf*. Should be simple to add, just haven't done it yet.
* *default*. Schemas can specify the default value to use for a field. Currently we just ignore these.
* *not*. Not sure how you could specify this in a type language. Needs more thoughts
* *additionalProperties*. Json schema lets you specify free-form properties too. These are unsupported 
for now (maybe we could take a Map of them if specified?)
* Any of the validation-only info, such as maxItems, since it doesn't contribute to the structure.


# Todo

There's still much that isn't 
