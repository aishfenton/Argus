# Argus

Scala macros for generating code from [Json Schemas](http://json-schema.org). Structures defined within the
schema (such as properties, enums, etc) are used to generate scala code at compile time.

Json encoder/decoders are also generated using the Circe Json library.

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

Many more examples [here](argus/src/test/scala/argus/macros/FromSchemaSpec.scala)

**NB:**
Why Argus? In keeping with the theme of Argonaut and Circe, Argus (son of Arestor) was the builder of the ship "Argo", 
on which the Argonauts sailed.

# Rules

## Supported constructs

### Object templates (i.e. classes)
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
</table>

### Definitions (i.e. common class definitions)
<table>
<tr><td>Json</td><td>Generated Scala</td>
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
</table>

### OneOf (i.e. type A or B) 
<table>
<tr><td>Json</td><td>Generated Scala</td>
<tr>
<td><pre>
{
  "oneOf": [
    { "$ref" : "#/definitions/Address" },
    { "type" : "number" }
  ]
}
</div></pre></td>
<td><pre>
@union sealed trait RootUnion
case class RootAddress(...) extends RootUnion
case class RootDouble(...) extends RootUnion
</pre></td>
</tr>
</table>

### Enums

<table>
<tr><td>Json</td><td>Generated Scala</td>
<tr>
<td><pre>
{
  "properties": { 
    "countries" : { "enum" : ["NZ", "US", "UK"] }
  }
}
</div></pre></td>
<td><pre>
@enum sealed trait Countries
object CountriesEnum {
  case object NZ(...) extends Countries
  case object US(...) extends Countries
  ...
}
case class Root(countries: Option[Countries] = None)
</pre></td>
</tr>
</table>

### Arrays 

<table>
<tr><td>Json</td><td>Generated Scala</td>
<tr>
<td><pre>
{
  "properties": { 
    "places" : { "items" : { "type": "string" } }
  }
}
</div></pre></td>
<td><pre>
case class Root(places: Option[List[String]] = None)
</pre></td>
</tr>
</table>

## Unsupported

* Only Circe encoders/decoders are supported, although the skeleton is laid out for adding support for other Json libraries.
* *anyOf* / *allOf*. Should be simple to add, just haven't done it yet.
* *default*. Schemas can specify the default value to use for a field. Currently we just ignore these.
* *not*. Not sure how you could specify this in a type language. Needs more thoughts
* *additionalProperties*, and *additionalItems*. Json schema lets you specify free-form properties too. These are unsupported 
for now (maybe we could take a Map of them if specified?)
* *patternProperties*. What should be the objects fields in this case? Maybe we just make it a Map?
* *dependencies*. Dependencies can also extend the schema... sigh.
* Any of the validation-only info, such as maxItems, since it doesn't contribute to the structure.

And lastly: We only generate code from json schemas, but we can't generate json-schema from code. This is fully possible, but 
requires work ;)

There's still a lot to do! Looking for contributors to address any of above.

# Usage Tips

1. All macros support arguments ```debug=true``` and ```outPath="..."```. ```debug``` causes the generated 
code to be dumped to stdout, and ```outPath``` causes the generated code to be written to a file.
    ```scala
    @fromSchemaResource("/simple.json", debug=true, outPath="/tmp/Simple.Scala")
    object Test
    ```

2. You can generate code from inline json schemas. Also supported are ```fromSchemaInputStream```
and ```fromSchemaURL``` too.
    ```scala
    @fromSchemaJson("""
    {
      "properties" : { 
        "name" : { "type" : "string" },
        "age"  : { "type" : "integer" }
      },
      "required" : ["name"]
    }
    """)
    object Schema
    ```

3. You can name the root class that is generated via the ```name="..."``` argument.
    ```scala
    @fromSchemaResource("/simple.json", name="Person")
    object Schema
    import Schema.Person
    ```

4. Within the object we also generate json encoder/decoder implicit variables, but you need to import 
them into scope. 
    ```scala
    @fromSchemaResource("/simple.json", name="Person")
    object Schema
    import Schema._
    import Schema.Implicits._
    
    Person(...).asJson
    ```
 