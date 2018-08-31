
package jbpark.jsonvalid

import jbpark.jsonvalid.JsonValidatorMeta._

import org.scalatest._


class NullConstraintSpec extends FreeSpec with Matchers {

  "NULL" - {
    val valueType = JsonSchema(
      REQ("null_test") -> NULL(ANY),
      REQ("피자") -> NULL(STR),
      REQ("치즈") -> NULL(ARRAY(null, null, NULL(JsonSchema(
        REQ("type") -> NULL(STR)
      ))))
    )

    "when the value is not null" - {
      val errors = JsonValidator(valueType, true).check("""
      {
        "null_test": 123,
        "피자": "콜라",
        "치즈": [{"type": "camembert"}, {"type": "brie"}]
      }
      """)

      "should never raise any error" in {
        errors shouldBe empty
      }
    }

    "when the value is null" - {
      val errors = JsonValidator(valueType, true).check("""
      {
        "null_test": null,
        "피자": null,
        "치즈": [{"type": "camembert"}, {"type": null}, null]
      }
      """)

      "should never raise any error" in {
        errors shouldBe empty
      }
    }
  }

  "NOT_NULL" - {
    val valueType = JsonSchema(
      REQ("null_test") -> NOT_NULL(ANY),
      REQ("피자") -> NOT_NULL(STR),
      REQ("치즈") -> NOT_NULL(ARRAY(null, null, NOT_NULL(JsonSchema(
        REQ("type") -> NOT_NULL(STR)
      ))))
    )

    "when the value is not null" - {
      val errors = JsonValidator(valueType, true).check("""
      {
        "null_test": 123,
        "피자": "콜라",
        "치즈": [{"type": "camembert"}, {"type": "brie"}]
      }
      """)

      "should never raise any error" in {
        errors shouldBe empty
      }
    }

    "when the value is null" - {
      val errors = JsonValidator(valueType, true).check("""
      {
        "null_test": null,
        "피자": null,
        "치즈": [{"type": "camembert"}, {"type": null}, null]
      }
      """)

      "should raise NullConstraintException" in {
        errors should have size 4
        errors(0) shouldBe a [NullConstraintException]
        errors(0) should have (
          'depth (Seq("null_test")),
        )
        errors(1) shouldBe a [NullConstraintException]
        errors(1) should have (
          'depth (Seq("피자")),
        )
        errors(2) shouldBe a [NullConstraintException]
        errors(2) should have (
          'depth (Seq("치즈[1]", "type")),
        )
        errors(3) shouldBe a [NullConstraintException]
        errors(3) should have (
          'depth (Seq("치즈[2]")),
        )
      }
    }
  }

}

