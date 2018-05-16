# Argus (船大工)

[![TravisCI](https://travis-ci.org/aishfenton/Argus.svg?branch=master)](https://travis-ci.org/aishfenton/Argus)

<img src="https://www.vegas-viz.org/images/argus-logo.png" width="170">

Scala macros for generating code from [Json Schemas](http://json-schema.org). Any structures defined within the
schema (such as properties, enums, etc) are used to generate scala code at compile time. Json encoder/decoders are also generated for the [Circe](https://github.com/travisbrown/circe) Json library.

**NB:**
Why Argus? In keeping with the theme of Argonaut and Circe, Argus (son of Arestor) was the builder of the ship "Argo", 
on which the Argonauts sailed.

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

**Many more examples [here](argus/src/test/scala/argus/macros/FromSchemaSpec.scala)**

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

### Basic types
<table>
<tr><td>json type</td><td>json format</td><td>Generated Scala type</td>
<tr><td><pre>string</pre></td><td><pre>*</pre></td><td><pre>String</pre></td></tr>
<tr><td><pre>string</pre></td><td><pre>uuid</pre></td><td><pre>java.util.UUID</pre></td></tr>
<tr><td><pre>string</pre></td><td><pre>date-time</pre></td><td><pre>java.time.ZonedDateTime</pre></td></tr>
<tr><td><pre>integer</pre></td><td><pre>* | int32</pre></td><td><pre>Int</pre></td></tr>
<tr><td><pre>integer</pre></td><td><pre>int64</pre></td><td><pre>Long</pre></td></tr>
<tr><td><pre>integer</pre></td><td><pre>int16</pre></td><td><pre>Short</pre></td></tr>
<tr><td><pre>integer</pre></td><td><pre>int8</pre></td><td><pre>Byte</pre></td></tr>
<tr><td><pre>number</pre></td><td><pre>* | double</pre></td><td><pre>Double</pre></td></tr>
<tr><td><pre>number</pre></td><td><pre>single</pre></td><td><pre>Float</pre></td></tr>
<tr><td><pre>boolean</pre></td><td><pre>*</pre></td><td><pre>Boolean</pre></td></tr>
<tr><td><pre>null</pre></td><td><pre>*</pre></td><td><pre>Null</pre></td></tr>
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

### Any Types (i.e. when a field can take arbitrary values)

<table>
<tr><td>Json</td><td>Generated Scala</td>
<tr>
<td><pre>
{
  "properties": { 
    "values" : { }
  }
}
</div></pre></td>
<td><pre>
case class Values(x: Any)
case class Root(values: Option[Values] = None)
</pre></td>
</tr>
</table>

## Unsupported

* Only Circe encoders/decoders are supported, although the skeleton is laid out for adding support for other Json libraries.
* *anyOf* / *allOf*. Should be simple to add, just haven't done it yet.
* *default*. Schemas can specify the default value to use for a field. Currently we just ignore these.
* *not*. Not sure how you could specify this in a type language. Needs more thoughts
* *additionalProperties*, and *additionalItems*. Json schema lets you specify free-form properties too. These are unsupported 
for now (although I think this should be easy now there's supprot for Any types)
* *patternProperties*. What should be the objects fields in this case? Maybe we just make it a Map?
* *dependencies*. Dependencies can also extend the schema... sigh.
* Any of the validation-only info, such as maxItems, since it doesn't contribute to the structure.

And lastly: We only generate code from json schemas, but we can't generate json-schema from code. This is fully possible, but 
requires work ;)

There's still a lot to do! Looking for contributors to address any of above.

# Usage Tips

1. All macros support arguments ```debug=true``` and ```outPath="..."```. ```debug``` causes the generated 
code to be dumped to stdout, and ```outPath``` causes the generated code to be written to a file. 
The optional argument ```outPathPackage``` allows to specify a package name for the output file.
 
    ```scala
    @fromSchemaResource("/simple.json", debug=true, outPath="/tmp/Simple.Scala", outPathPackage="argus.simple")
    object Test
    ```

3. You can generate code from inline json schemas. Also supported are ```fromSchemaInputStream```
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

4. You can name the root class that is generated via the ```name="..."``` argument.
 
    ```scala
    @fromSchemaResource("/simple.json", name="Person")
    object Schema
    import Schema.Person
    ```

5. Within the object we also generate json encoder/decoder implicit variables, but you need to import 
them into scope. 

    ```scala
    @fromSchemaResource("/simple.json", name="Person")
    object Schema
    import Schema._
    import Schema.Implicits._
    
    Person(...).asJson
    ```
6. You can override specific Encoders/Decoders. All implicits are baked into a trait called LowPriorityImplicits.
Rather than importing Foo.Implicits you can make your own implicits object that extends this and provides overrides.
For example:

    ```scala
    @fromSchemaResource("/simple.json")
    object Foo
    import Foo._

    object BetterImplicits extends Foo.LowPriorityImplicits {
      implicit val myEncoder: Encoder[Foo.Root] =   ... 
      implicit val betterDecoder: Decoder[Foo.Root] = ...
    }
    import BetterImplicits._
    ```

7. Free form Json (we call them Any types above) are quite common within Json schemas. These are fields that are left open to take any
kind of Json chunk (maybe for additional properties, or data, etc). Unfortunately they presents a challenge in a strongly typed 
language, such as Scala, where we always need some kind of type. 
 
    The approach we've taken is to wrap these chunks in their own case class which has a single field of type ```Any```. 
    This also allows you to override the encode/decoders for that type (```Root.Data``` in this example) with something more custom
    if required.
    
    ```scala
    @fromSchemaJson("""
    {
      "type": "object",
      "properties" : { 
        "data" : { "type": "array", "items": { } }
      }
    }
    """)
    object Schema
    import Schema._
    import Schema.Implicits._
    
    val values = List( Root.Data(Map("size" -> 350, "country" -> "US")), Root.Data(Map("size" -> 350, "country" -> "US")) )
    Root(data=Some(values))
    ```
    
    The default encoder/decoder (as shown in the code example above) works if your types are:
    
      * Primitive types: Boolean, Byte, Short, Int, Long, Float, Double
      * Primate arrays (Array[Byte], Array[Int], etc)
      * Seq\[Any\] (and subclasses) where Any needs to be one of the types in this list
      * Maps[String, Any], where Any needs to be one of the types in this list.
      
    Or, in other words, you can't stick arbitrary objects in the Any wrapper and expect their encoders/decoders to get picked up. 
    If you need that then you'll have to override the default encoder/decoder for this type. 

