
package jbpark.jsonvalid

import jbpark.jsonvalid.JsonValidatorMeta._

import org.scalatest._


class DomainConstraintSpec extends FreeSpec with Matchers {

  "ALLOW_VAL" - {
    val valueType = ARRAY(null, null, NULL(JsonSchema(
      REQ("data") -> NULL(ALLOW_VAL("hello", 12, 0.34, true, "0"))
    )))

    "when the value is one of the given values" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"data": "hello"},
        {"data": 12},
        {"data": 0.34},
        {"data": true},
        {"data": "0"}
      ]
      """)

      "should never raise any error" in {
        errors shouldBe empty
      }
    }

    "when the value is not in the given values" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"data": ""},
        {"data": "12"},
        {"data": "0.34"},
        {"data": false},
        {"data": 0}
      ]
      """)

      "should raise DomainConstraintException" in {
        errors should have size 5
        errors(0) shouldBe a [DomainConstraintException]
        errors(0) should have (
          'depth          (Seq("[0]", "data")),
          'expectedValues (Seq("hello", 12, 0.34, true, "0")),
        )
        errors(1) shouldBe a [DomainConstraintException]
        errors(1) should have (
          'depth          (Seq("[1]", "data")),
          'expectedValues (Seq("hello", 12, 0.34, true, "0")),
        )
        errors(2) shouldBe a [DomainConstraintException]
        errors(2) should have (
          'depth          (Seq("[2]", "data")),
          'expectedValues (Seq("hello", 12, 0.34, true, "0")),
        )
        errors(3) shouldBe a [DomainConstraintException]
        errors(3) should have (
          'depth          (Seq("[3]", "data")),
          'expectedValues (Seq("hello", 12, 0.34, true, "0")),
        )
        errors(4) shouldBe a [DomainConstraintException]
        errors(4) should have (
          'depth          (Seq("[4]", "data")),
          'expectedValues (Seq("hello", 12, 0.34, true, "0")),
        )
      }
    }
  }

  "Y_OR_N" - {
    val valueType = ARRAY(null, null, NULL(JsonSchema(
      REQ("data") -> NULL(Y_OR_N)
    )))

    "when the value is Y or N" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"data": "Y"},
        {"data": "N"}
      ]
      """)

      "should never raise any error" in {
        errors shouldBe empty
      }
    }

    "when the value is neither Y nor N" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"data": "y"},
        {"data": "n"},
        {"data": ""}
      ]
      """)

      "should raise DomainConstraintException" in {
        errors should have size 3
        errors(0) shouldBe a [DomainConstraintException]
        errors(0) should have (
          'depth          (Seq("[0]", "data")),
          'expectedValues (Seq("Y", "N")),
        )
        errors(1) shouldBe a [DomainConstraintException]
        errors(1) should have (
          'depth          (Seq("[1]", "data")),
          'expectedValues (Seq("Y", "N")),
        )
        errors(2) shouldBe a [DomainConstraintException]
        errors(2) should have (
          'depth          (Seq("[2]", "data")),
          'expectedValues (Seq("Y", "N")),
        )
      }
    }
  }

  "ANY_BOOL" - {
    val valueType = ARRAY(null, null, NULL(JsonSchema(
      REQ("data") -> NULL(ANY_BOOL)
    )))

    "when the value is one of boolean values" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"data": "1"},
        {"data": "0"},
        {"data": 1},
        {"data": 0},
        {"data": true},
        {"data": false}
      ]
      """)

      "should never raise any error" in {
        errors shouldBe empty
      }
    }

    "when the value is not in boolean values" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"data": ""},
        {"data": "true"},
        {"data": "false"}
      ]
      """)

      "should raise DomainConstraintException" in {
        errors should have size 3
        errors(0) shouldBe a [DomainConstraintException]
        errors(0) should have (
          'depth          (Seq("[0]", "data")),
          'expectedValues (Seq("1", "0", 1, 0, true, false)),
        )
        errors(1) shouldBe a [DomainConstraintException]
        errors(1) should have (
          'depth          (Seq("[1]", "data")),
          'expectedValues (Seq("1", "0", 1, 0, true, false)),
        )
        errors(2) shouldBe a [DomainConstraintException]
        errors(2) should have (
          'depth          (Seq("[2]", "data")),
          'expectedValues (Seq("1", "0", 1, 0, true, false)),
        )
      }
    }
  }

}

