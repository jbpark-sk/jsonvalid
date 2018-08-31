
package jbpark.jsonvalid

import jbpark.jsonvalid.JsonValidatorMeta._

import org.scalatest._


class JsonValidatorSpec extends FreeSpec with Matchers {

  "JsonValidator" - {
    val valueType = JsonSchema(
      REQ("hello", "world", "foo", "bar") -> NOT_NULL(STR)
    )

    "when collectMultipleErrors is true" - {
      val errors = JsonValidator(valueType, true).check("""
      {
        "world": null,
        "foo": 0,
        "bar": ""
      }
      """)

      "should raise errors as many as possible" in {
        errors should have size 3
      }
    }

    "when collectMultipleErrors is false" - {
      val errors = JsonValidator(valueType, false).check("""
      {
        "world": null,
        "foo": 0,
        "bar": ""
      }
      """)

      "should raise only one error" in {
        errors should have size 1
      }
    }
  }

}

