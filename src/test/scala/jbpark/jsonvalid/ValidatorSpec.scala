
package jbpark.jsonvalid

import jbpark.jsonvalid.JsonValidatorMeta._

import org.json4s.JString
import org.scalatest._


class ValidatorSpec extends FreeSpec with Matchers {

  "STR_CHECK" - {
    val valueType = ARRAY(null, null, NULL(JsonSchema(
      REQ("data") -> NULL(STR_CHECK(_.isEmpty))
    )))

    "when the given validator returns true" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"data": ""},
        {"data": ""}
      ]
      """)

      "should never raise any error" in {
        errors shouldBe empty
      }
    }

    "when the given validator returns false" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"data": " "},
        {"data": "hello"}
      ]
      """)

      "should raise ValidationFailedException" in {
        errors should have size 2
        errors(0) shouldBe a [ValidationFailedException]
        errors(0) should have (
          'depth (Seq("[0]", "data")),
        )
        errors(1) shouldBe a [ValidationFailedException]
        errors(1) should have (
          'depth (Seq("[1]", "data")),
        )
      }
    }

    "when the type is not JString" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"data": [""]},
        {"data": 0},
        {"data": 0.0},
        {"data": false}
      ]
      """)

      "should raise TypeErrorException" in {
        errors should have size 4
        errors(0) shouldBe a [TypeErrorException]
        errors(0) should have (
          'depth        (Seq("[0]", "data")),
          'expectedType ("STR_CHECK"),
        )
        errors(1) shouldBe a [TypeErrorException]
        errors(1) should have (
          'depth        (Seq("[1]", "data")),
          'expectedType ("STR_CHECK"),
        )
        errors(2) shouldBe a [TypeErrorException]
        errors(2) should have (
          'depth        (Seq("[2]", "data")),
          'expectedType ("STR_CHECK"),
        )
        errors(3) shouldBe a [TypeErrorException]
        errors(3) should have (
          'depth        (Seq("[3]", "data")),
          'expectedType ("STR_CHECK"),
        )
      }
    }
  }

  "INT_CHECK" - {
    val valueType = ARRAY(null, null, NULL(JsonSchema(
      REQ("data") -> NULL(INT_CHECK(n => (n & 1) == 1))
    )))

    "when the given validator returns true" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"data": 1},
        {"data": 3},
        {"data": 5},
        {"data": 12345},
        {"data": -123}
      ]
      """)

      "should never raise any error" in {
        errors shouldBe empty
      }
    }

    "when the given validator returns false" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"data": 2},
        {"data": 10},
        {"data": -10}
      ]
      """)

      "should raise ValidationFailedException" in {
        errors should have size 3
        errors(0) shouldBe a [ValidationFailedException]
        errors(0) should have (
          'depth (Seq("[0]", "data")),
        )
        errors(1) shouldBe a [ValidationFailedException]
        errors(1) should have (
          'depth (Seq("[1]", "data")),
        )
        errors(2) shouldBe a [ValidationFailedException]
        errors(2) should have (
          'depth (Seq("[2]", "data")),
        )
      }
    }

    "when the type is not JInt" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"data": ""},
        {"data": [0]},
        {"data": 0.0},
        {"data": false}
      ]
      """)

      "should raise TypeErrorException" in {
        errors should have size 4
        errors(0) shouldBe a [TypeErrorException]
        errors(0) should have (
          'depth        (Seq("[0]", "data")),
          'expectedType ("INT_CHECK"),
        )
        errors(1) shouldBe a [TypeErrorException]
        errors(1) should have (
          'depth        (Seq("[1]", "data")),
          'expectedType ("INT_CHECK"),
        )
        errors(2) shouldBe a [TypeErrorException]
        errors(2) should have (
          'depth        (Seq("[2]", "data")),
          'expectedType ("INT_CHECK"),
        )
        errors(3) shouldBe a [TypeErrorException]
        errors(3) should have (
          'depth        (Seq("[3]", "data")),
          'expectedType ("INT_CHECK"),
        )
      }
    }
  }

  "DBL_CHECK" - {
    val valueType = ARRAY(null, null, NULL(JsonSchema(
      REQ("data") -> NULL(DBL_CHECK(n => n >= 0.0 && n < 1.0))
    )))

    "when the given validator returns true" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"data": 0.0},
        {"data": 0.01},
        {"data": 0.5},
        {"data": 0.99},
        {"data": 0.999}
      ]
      """)

      "should never raise any error" in {
        errors shouldBe empty
      }
    }

    "when the given validator returns false" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"data": -1.0},
        {"data": 1.0}
      ]
      """)

      "should raise ValidationFailedException" in {
        errors should have size 2
        errors(0) shouldBe a [ValidationFailedException]
        errors(0) should have (
          'depth (Seq("[0]", "data")),
        )
        errors(1) shouldBe a [ValidationFailedException]
        errors(1) should have (
          'depth (Seq("[1]", "data")),
        )
      }
    }

    "when the type is not JDouble" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"data": ""},
        {"data": 0},
        {"data": [0.0]},
        {"data": false}
      ]
      """)

      "should raise TypeErrorException" in {
        errors should have size 4
        errors(0) shouldBe a [TypeErrorException]
        errors(0) should have (
          'depth        (Seq("[0]", "data")),
          'expectedType ("DBL_CHECK"),
        )
        errors(1) shouldBe a [TypeErrorException]
        errors(1) should have (
          'depth        (Seq("[1]", "data")),
          'expectedType ("DBL_CHECK"),
        )
        errors(2) shouldBe a [TypeErrorException]
        errors(2) should have (
          'depth        (Seq("[2]", "data")),
          'expectedType ("DBL_CHECK"),
        )
        errors(3) shouldBe a [TypeErrorException]
        errors(3) should have (
          'depth        (Seq("[3]", "data")),
          'expectedType ("DBL_CHECK"),
        )
      }
    }
  }

  "BOOL_CHECK" - {
    val valueType = ARRAY(null, null, NULL(JsonSchema(
      REQ("data") -> NULL(BOOL_CHECK(_ == false))
    )))

    "when the given validator returns true" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"data": false},
        {"data": false}
      ]
      """)

      "should never raise any error" in {
        errors shouldBe empty
      }
    }

    "when the given validator returns false" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"data": true},
        {"data": true}
      ]
      """)

      "should raise ValidationFailedException" in {
        errors should have size 2
        errors(0) shouldBe a [ValidationFailedException]
        errors(0) should have (
          'depth (Seq("[0]", "data")),
        )
        errors(1) shouldBe a [ValidationFailedException]
        errors(1) should have (
          'depth (Seq("[1]", "data")),
        )
      }
    }

    "when the type is not JBoolean" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"data": ""},
        {"data": 0},
        {"data": 0.0},
        {"data": [false]}
      ]
      """)

      "should raise TypeErrorException" in {
        errors should have size 4
        errors(0) shouldBe a [TypeErrorException]
        errors(0) should have (
          'depth        (Seq("[0]", "data")),
          'expectedType ("BOOL_CHECK"),
        )
        errors(1) shouldBe a [TypeErrorException]
        errors(1) should have (
          'depth        (Seq("[1]", "data")),
          'expectedType ("BOOL_CHECK"),
        )
        errors(2) shouldBe a [TypeErrorException]
        errors(2) should have (
          'depth        (Seq("[2]", "data")),
          'expectedType ("BOOL_CHECK"),
        )
        errors(3) shouldBe a [TypeErrorException]
        errors(3) should have (
          'depth        (Seq("[3]", "data")),
          'expectedType ("BOOL_CHECK"),
        )
      }
    }
  }

  "ANY_CHECK" - {
    val valueType = ARRAY(null, null, NULL(JsonSchema(
      REQ("data") -> NULL(ANY_CHECK {
        case JString(_) => true
        case _ => false
      })
    )))

    "when the given validator returns true" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"data": "hello"},
        {"data": "world"},
        {"data": ""}
      ]
      """)

      "should never raise any error" in {
        errors shouldBe empty
      }
    }

    "when the given validator returns false" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"data": 12},
        {"data": 3.4},
        {"data": false}
      ]
      """)

      "should raise ValidationFailedException" in {
        errors should have size 3
        errors(0) shouldBe a [ValidationFailedException]
        errors(0) should have (
          'depth (Seq("[0]", "data")),
        )
        errors(1) shouldBe a [ValidationFailedException]
        errors(1) should have (
          'depth (Seq("[1]", "data")),
        )
        errors(2) shouldBe a [ValidationFailedException]
        errors(2) should have (
          'depth (Seq("[2]", "data")),
        )
      }
    }
  }

}

