
package jbpark.jsonvalid

import jbpark.jsonvalid.JsonValidatorMeta._

import org.scalatest._


class JsonSchemaSpec extends FreeSpec with Matchers {

  "JsonSchema" - {
    val valueType = JsonSchema(
      REQ("a") -> NULL(JsonSchema(
        REQ("b") -> NULL(JsonSchema(
          REQ("c") -> NULL(JsonSchema(
            REQ("d") -> NULL(ANY)
          ))
        ))
      ))
    )

    "when the type is JObject" - {
      val errors = JsonValidator(valueType, true).check("""
      {
        "a": {
          "b": {
            "c": {
              "d": 0
            }
          }
        }
      }
      """)

      "should never raise any error" in {
        errors shouldBe empty
      }
    }

    "when the type is not JObject" - {
      val errors = JsonValidator(valueType, true).check("""
      {
        "a": {
          "b": 0
        }
      }
      """)

      "should raise TypeErrorException" in {
        errors should have size 1
        errors(0) shouldBe a [TypeErrorException]
        errors(0) should have (
          'depth        (Seq("a", "b")),
          'expectedType ("JsonSchema"),
        )
      }
    }
  }

}

