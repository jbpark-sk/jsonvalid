
package jbpark.jsonvalid

import jbpark.jsonvalid.JsonValidatorMeta._

import org.scalatest._


class PrimitiveTypeSpec extends FreeSpec with Matchers {

  "STR" - {
    val valueType = ARRAY(null, null, NULL(JsonSchema(
      REQ("name") -> NULL(STR)
    )))

    "when the type is JString" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"name": "alice"},
        {"name": "bob"}
      ]
      """)

      "should never raise any error" in {
        errors shouldBe empty
      }
    }

    "when the type is not JString" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"name": 0},
        {"name": false},
        {"name": 12.34},
        {"name": ["john"]}
      ]
      """)

      "should raise TypeErrorException" in {
        errors should have size 4
        errors(0) shouldBe a [TypeErrorException]
        errors(0) should have (
          'depth        (Seq("[0]", "name")),
          'expectedType ("STR"),
        )
        errors(1) shouldBe a [TypeErrorException]
        errors(1) should have (
          'depth        (Seq("[1]", "name")),
          'expectedType ("STR"),
        )
        errors(2) shouldBe a [TypeErrorException]
        errors(2) should have (
          'depth        (Seq("[2]", "name")),
          'expectedType ("STR"),
        )
        errors(3) shouldBe a [TypeErrorException]
        errors(3) should have (
          'depth        (Seq("[3]", "name")),
          'expectedType ("STR"),
        )
      }
    }
  }

  "INT" - {
    val valueType = ARRAY(null, null, NULL(JsonSchema(
      REQ("age") -> NULL(INT)
    )))

    "when the type is JInt" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"age": 12},
        {"age": 34}
      ]
      """)

      "should never raise any error" in {
        errors shouldBe empty
      }
    }

    "when the type is not JInt" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"age": "john"},
        {"age": false},
        {"age": 12.34},
        {"age": [0]}
      ]
      """)

      "should raise TypeErrorException" in {
        errors should have size 4
        errors(0) shouldBe a [TypeErrorException]
        errors(0) should have (
          'depth        (Seq("[0]", "age")),
          'expectedType ("INT"),
        )
        errors(1) shouldBe a [TypeErrorException]
        errors(1) should have (
          'depth        (Seq("[1]", "age")),
          'expectedType ("INT"),
        )
        errors(2) shouldBe a [TypeErrorException]
        errors(2) should have (
          'depth        (Seq("[2]", "age")),
          'expectedType ("INT"),
        )
        errors(3) shouldBe a [TypeErrorException]
        errors(3) should have (
          'depth        (Seq("[3]", "age")),
          'expectedType ("INT"),
        )
      }
    }
  }

  "DBL" - {
    val valueType = ARRAY(null, null, NULL(JsonSchema(
      REQ("score") -> NULL(DBL)
    )))

    "when the type is JDouble" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"score": 0.12},
        {"score": 34.56}
      ]
      """)

      "should never raise any error" in {
        errors shouldBe empty
      }
    }

    "when the type is not JDouble" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"score": "john"},
        {"score": false},
        {"score": [12.34]},
        {"score": 12}
      ]
      """)

      "should raise TypeErrorException" in {
        errors should have size 4
        errors(0) shouldBe a [TypeErrorException]
        errors(0) should have (
          'depth        (Seq("[0]", "score")),
          'expectedType ("DBL"),
        )
        errors(1) shouldBe a [TypeErrorException]
        errors(1) should have (
          'depth        (Seq("[1]", "score")),
          'expectedType ("DBL"),
        )
        errors(2) shouldBe a [TypeErrorException]
        errors(2) should have (
          'depth        (Seq("[2]", "score")),
          'expectedType ("DBL"),
        )
        errors(3) shouldBe a [TypeErrorException]
        errors(3) should have (
          'depth        (Seq("[3]", "score")),
          'expectedType ("DBL"),
        )
      }
    }
  }

  "BOOL" - {
    val valueType = ARRAY(null, null, NULL(JsonSchema(
      REQ("married") -> NULL(BOOL)
    )))

    "when the type is JBoolean" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"married": true},
        {"married": false}
      ]
      """)

      "should never raise any error" in {
        errors shouldBe empty
      }
    }

    "when the type is not JBoolean" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"married": "john"},
        {"married": [false]},
        {"married": 12.34},
        {"married": 12}
      ]
      """)

      "should raise TypeErrorException" in {
        errors should have size 4
        errors(0) shouldBe a [TypeErrorException]
        errors(0) should have (
          'depth        (Seq("[0]", "married")),
          'expectedType ("BOOL"),
        )
        errors(1) shouldBe a [TypeErrorException]
        errors(1) should have (
          'depth        (Seq("[1]", "married")),
          'expectedType ("BOOL"),
        )
        errors(2) shouldBe a [TypeErrorException]
        errors(2) should have (
          'depth        (Seq("[2]", "married")),
          'expectedType ("BOOL"),
        )
        errors(3) shouldBe a [TypeErrorException]
        errors(3) should have (
          'depth        (Seq("[3]", "married")),
          'expectedType ("BOOL"),
        )
      }
    }
  }

  "ANY" - {
    val valueType = JsonSchema(
      REQ("anything") -> NULL(ANY)
    )

    "when the type is JString" - {
      val errors = JsonValidator(valueType, true).check("""
      {
        "anything": "hello"
      }
      """)

      "should never raise any error" in {
        errors shouldBe empty
      }
    }

    "when the type is JInt" - {
      val errors = JsonValidator(valueType, true).check("""
      {
        "anything": 12345
      }
      """)

      "should never raise any error" in {
        errors shouldBe empty
      }
    }

    "when the type is JDouble" - {
      val errors = JsonValidator(valueType, true).check("""
      {
        "anything": 0.12345
      }
      """)

      "should never raise any error" in {
        errors shouldBe empty
      }
    }

    "when the type is JBoolean" - {
      val errors = JsonValidator(valueType, true).check("""
      {
        "anything": true
      }
      """)

      "should never raise any error" in {
        errors shouldBe empty
      }
    }

    "when the type is JNull" - {
      val errors = JsonValidator(valueType, true).check("""
      {
        "anything": null
      }
      """)

      "should never raise any error" in {
        errors shouldBe empty
      }
    }

    "when the type is JArray" - {
      val errors = JsonValidator(valueType, true).check("""
      {
        "anything": [true, false]
      }
      """)

      "should never raise any error" in {
        errors shouldBe empty
      }
    }

    "when the type is JObject" - {
      val errors = JsonValidator(valueType, true).check("""
      {
        "anything": {"you": "want"}
      }
      """)

      "should never raise any error" in {
        errors shouldBe empty
      }
    }
  }

  "STR_LEN" - {
    val valueType = ARRAY(null, null, NULL(JsonSchema(
      REQ("a") -> NULL(STR_LEN(null, null)),
      REQ("b") -> NULL(STR_LEN(1, null)),
      REQ("c") -> NULL(STR_LEN(null, 5)),
      REQ("d") -> NULL(STR_LEN(2, 4))
    )))

    "when the type is JString and the length does not exceed the limit" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"a": "", "b": "x", "c": "", "d": "xx"},
        {"a": "xxxxx", "b": "xxxxx", "c": "xxxxx", "d": "xxxx"}
      ]
      """)

      "should never raise any error" in {
        errors shouldBe empty
      }
    }

    "when the type is not JString" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"a": [""], "b": 0, "c": false, "d": 0.0}
      ]
      """)

      "should raise TypeErrorException" in {
        errors should have size 4
        errors(0) shouldBe a [TypeErrorException]
        errors(0) should have (
          'depth        (Seq("[0]", "a")),
          'expectedType ("STR_LEN(null,null)"),
        )
        errors(1) shouldBe a [TypeErrorException]
        errors(1) should have (
          'depth        (Seq("[0]", "b")),
          'expectedType ("STR_LEN(1,null)"),
        )
        errors(2) shouldBe a [TypeErrorException]
        errors(2) should have (
          'depth        (Seq("[0]", "c")),
          'expectedType ("STR_LEN(null,5)"),
        )
        errors(3) shouldBe a [TypeErrorException]
        errors(3) should have (
          'depth        (Seq("[0]", "d")),
          'expectedType ("STR_LEN(2,4)"),
        )
      }
    }

    "when the length exceeds the lower limit" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"a": "", "b": "", "c": "", "d": ""}
      ]
      """)

      "should raise StringLengthException" in {
        errors should have size 2
        errors(0) shouldBe a [StringLengthException]
        errors(0) should have (
          'depth          (Seq("[0]", "b")),
          'expectedLength ("1~null"),
        )
        errors(1) shouldBe a [StringLengthException]
        errors(1) should have (
          'depth          (Seq("[0]", "d")),
          'expectedLength ("2~4"),
        )
      }
    }

    "when the length exceeds the upper limit" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"a": "xxxxxx", "b": "xxxxxx", "c": "xxxxxx", "d": "xxxxxx"}
      ]
      """)

      "should raise StringLengthException" in {
        errors should have size 2
        errors(0) shouldBe a [StringLengthException]
        errors(0) should have (
          'depth          (Seq("[0]", "c")),
          'expectedLength ("null~5"),
        )
        errors(1) shouldBe a [StringLengthException]
        errors(1) should have (
          'depth          (Seq("[0]", "d")),
          'expectedLength ("2~4"),
        )
      }
    }
  }

  "INT_RANGE" - {
    val valueType = ARRAY(null, null, NULL(JsonSchema(
      REQ("a") -> NULL(INT_RANGE(null, null)),
      REQ("b") -> NULL(INT_RANGE(-10, null)),
      REQ("c") -> NULL(INT_RANGE(null, 10)),
      REQ("d") -> NULL(INT_RANGE(-5, 5))
    )))

    "when the type is JInt and the range does not exceed the limit" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"a": -10000, "b": -10, "c": -10000, "d": -5},
        {"a": 10000, "b": 10000, "c": 10, "d": 5}
      ]
      """)

      "should never raise any error" in {
        errors shouldBe empty
      }
    }

    "when the type is not JInt" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"a": [0], "b": "", "c": false, "d": 0.0}
      ]
      """)

      "should raise TypeErrorException" in {
        errors should have size 4
        errors(0) shouldBe a [TypeErrorException]
        errors(0) should have (
          'depth        (Seq("[0]", "a")),
          'expectedType ("INT_RANGE(null,null)"),
        )
        errors(1) shouldBe a [TypeErrorException]
        errors(1) should have (
          'depth        (Seq("[0]", "b")),
          'expectedType ("INT_RANGE(-10,null)"),
        )
        errors(2) shouldBe a [TypeErrorException]
        errors(2) should have (
          'depth        (Seq("[0]", "c")),
          'expectedType ("INT_RANGE(null,10)"),
        )
        errors(3) shouldBe a [TypeErrorException]
        errors(3) should have (
          'depth        (Seq("[0]", "d")),
          'expectedType ("INT_RANGE(-5,5)"),
        )
      }
    }

    "when the range exceeds the lower limit" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"a": -10000, "b": -11, "c": -10000, "d": -6}
      ]
      """)

      "should raise NumberRangeException" in {
        errors should have size 2
        errors(0) shouldBe a [NumberRangeException]
        errors(0) should have (
          'depth         (Seq("[0]", "b")),
          'expectedRange ("-10~null"),
        )
        errors(1) shouldBe a [NumberRangeException]
        errors(1) should have (
          'depth         (Seq("[0]", "d")),
          'expectedRange ("-5~5"),
        )
      }
    }

    "when the range exceeds the upper limit" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"a": 10000, "b": 10000, "c": 11, "d": 6}
      ]
      """)

      "should raise NumberRangeException" in {
        errors should have size 2
        errors(0) shouldBe a [NumberRangeException]
        errors(0) should have (
          'depth         (Seq("[0]", "c")),
          'expectedRange ("null~10"),
        )
        errors(1) shouldBe a [NumberRangeException]
        errors(1) should have (
          'depth         (Seq("[0]", "d")),
          'expectedRange ("-5~5"),
        )
      }
    }
  }

  "DBL_RANGE" - {
    val valueType = ARRAY(null, null, NULL(JsonSchema(
      REQ("a") -> NULL(DBL_RANGE(null, null)),
      REQ("b") -> NULL(DBL_RANGE(-1.0, null)),
      REQ("c") -> NULL(DBL_RANGE(null, 1.0)),
      REQ("d") -> NULL(DBL_RANGE(-0.5, 0.5))
    )))

    "when the type is JDouble and the range does not exceed the limit" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"a": -10000.0, "b": -1.0, "c": -10000.0, "d": -0.5},
        {"a": 10000.0, "b": 10000.0, "c": 1.0, "d": 0.5}
      ]
      """)

      "should never raise any error" in {
        errors shouldBe empty
      }
    }

    "when the type is not JDouble" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"a": 0, "b": "", "c": false, "d": [0.0]}
      ]
      """)

      "should raise TypeErrorException" in {
        errors should have size 4
        errors(0) shouldBe a [TypeErrorException]
        errors(0) should have (
          'depth        (Seq("[0]", "a")),
          'expectedType ("DBL_RANGE(null,null)"),
        )
        errors(1) shouldBe a [TypeErrorException]
        errors(1) should have (
          'depth        (Seq("[0]", "b")),
          'expectedType ("DBL_RANGE(-1.0,null)"),
        )
        errors(2) shouldBe a [TypeErrorException]
        errors(2) should have (
          'depth        (Seq("[0]", "c")),
          'expectedType ("DBL_RANGE(null,1.0)"),
        )
        errors(3) shouldBe a [TypeErrorException]
        errors(3) should have (
          'depth        (Seq("[0]", "d")),
          'expectedType ("DBL_RANGE(-0.5,0.5)"),
        )
      }
    }

    "when the range exceeds the lower limit" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"a": -10000.0, "b": -1.1, "c": -10000.0, "d": -0.6}
      ]
      """)

      "should raise NumberRangeException" in {
        errors should have size 2
        errors(0) shouldBe a [NumberRangeException]
        errors(0) should have (
          'depth         (Seq("[0]", "b")),
          'expectedRange ("-1.0~null"),
        )
        errors(1) shouldBe a [NumberRangeException]
        errors(1) should have (
          'depth         (Seq("[0]", "d")),
          'expectedRange ("-0.5~0.5"),
        )
      }
    }

    "when the range exceeds the upper limit" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"a": 10000.0, "b": 10000.0, "c": 1.1, "d": 0.6}
      ]
      """)

      "should raise NumberRangeException" in {
        errors should have size 2
        errors(0) shouldBe a [NumberRangeException]
        errors(0) should have (
          'depth         (Seq("[0]", "c")),
          'expectedRange ("null~1.0"),
        )
        errors(1) shouldBe a [NumberRangeException]
        errors(1) should have (
          'depth         (Seq("[0]", "d")),
          'expectedRange ("-0.5~0.5"),
        )
      }
    }
  }

}

