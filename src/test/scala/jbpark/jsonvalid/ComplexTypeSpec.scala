
package jbpark.jsonvalid

import jbpark.jsonvalid.JsonValidatorMeta._

import org.scalatest._


class ComplexTypeSpec extends FreeSpec with Matchers {

  "REAL" - {
    val valueType = ARRAY(null, null, NULL(JsonSchema(
      REQ("score") -> NULL(REAL)
    )))

    "when the type is JInt" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"score": 12},
        {"score": -34},
        {"score": 0}
      ]
      """)

      "should never raise any error" in {
        errors shouldBe empty
      }
    }

    "when the type is JDouble" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"score": 12.34},
        {"score": -56.78},
        {"score": 0.0}
      ]
      """)

      "should never raise any error" in {
        errors shouldBe empty
      }
    }

    "when the type is neither JInt nor JDouble" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"score": "12"},
        {"score": [0]},
        {"score": [12.34]},
        {"score": false}
      ]
      """)

      "should raise TypeErrorException" in {
        errors should have size 4
        errors(0) shouldBe a [TypeErrorException]
        errors(0) should have (
          'depth        (Seq("[0]", "score")),
          'expectedType ("REAL_RANGE(null,null)"),
        )
        errors(1) shouldBe a [TypeErrorException]
        errors(1) should have (
          'depth        (Seq("[1]", "score")),
          'expectedType ("REAL_RANGE(null,null)"),
        )
        errors(2) shouldBe a [TypeErrorException]
        errors(2) should have (
          'depth        (Seq("[2]", "score")),
          'expectedType ("REAL_RANGE(null,null)"),
        )
        errors(3) shouldBe a [TypeErrorException]
        errors(3) should have (
          'depth        (Seq("[3]", "score")),
          'expectedType ("REAL_RANGE(null,null)"),
        )
      }
    }
  }

  "STR_INT" - {
    val valueType = ARRAY(null, null, NULL(JsonSchema(
      REQ("age") -> NULL(STR_INT)
    )))

    "when the type is JInt" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"age": 12},
        {"age": -34},
        {"age": 0}
      ]
      """)

      "should never raise any error" in {
        errors shouldBe empty
      }
    }

    "when the type is JString and it can be converted into JInt" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"age": "12"},
        {"age": "-34"},
        {"age": "0"}
      ]
      """)

      "should never raise any error" in {
        errors shouldBe empty
      }
    }

    "when the type is neither JInt nor JString" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"age": ["12"]},
        {"age": [0]},
        {"age": 12.34},
        {"age": false}
      ]
      """)

      "should raise TypeErrorException" in {
        errors should have size 4
        errors(0) shouldBe a [TypeErrorException]
        errors(0) should have (
          'depth        (Seq("[0]", "age")),
          'expectedType ("STR_INT_RANGE(null,null)"),
        )
        errors(1) shouldBe a [TypeErrorException]
        errors(1) should have (
          'depth        (Seq("[1]", "age")),
          'expectedType ("STR_INT_RANGE(null,null)"),
        )
        errors(2) shouldBe a [TypeErrorException]
        errors(2) should have (
          'depth        (Seq("[2]", "age")),
          'expectedType ("STR_INT_RANGE(null,null)"),
        )
        errors(3) shouldBe a [TypeErrorException]
        errors(3) should have (
          'depth        (Seq("[3]", "age")),
          'expectedType ("STR_INT_RANGE(null,null)"),
        )
      }
    }

    "when the type is JString but it cannot be converted into JInt" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"age": ""},
        {"age": "hello"},
        {"age": "1.0"},
        {"age": "abc0"},
        {"age": "0abc"}
      ]
      """)

      "should raise TypeErrorException" in {
        errors should have size 5
        errors(0) shouldBe a [TypeErrorException]
        errors(0) should have (
          'depth        (Seq("[0]", "age")),
          'expectedType ("STR_INT_RANGE(null,null)"),
        )
        errors(1) shouldBe a [TypeErrorException]
        errors(1) should have (
          'depth        (Seq("[1]", "age")),
          'expectedType ("STR_INT_RANGE(null,null)"),
        )
        errors(2) shouldBe a [TypeErrorException]
        errors(2) should have (
          'depth        (Seq("[2]", "age")),
          'expectedType ("STR_INT_RANGE(null,null)"),
        )
        errors(3) shouldBe a [TypeErrorException]
        errors(3) should have (
          'depth        (Seq("[3]", "age")),
          'expectedType ("STR_INT_RANGE(null,null)"),
        )
        errors(4) shouldBe a [TypeErrorException]
        errors(4) should have (
          'depth        (Seq("[4]", "age")),
          'expectedType ("STR_INT_RANGE(null,null)"),
        )
      }
    }
  }

  "STR_REAL" - {
    val valueType = ARRAY(null, null, NULL(JsonSchema(
      REQ("score") -> NULL(STR_REAL)
    )))

    "when the type is JInt" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"score": 12},
        {"score": -34},
        {"score": 0}
      ]
      """)

      "should never raise any error" in {
        errors shouldBe empty
      }
    }

    "when the type is JDouble" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"score": 12.34},
        {"score": -56.78},
        {"score": 0.0}
      ]
      """)

      "should never raise any error" in {
        errors shouldBe empty
      }
    }

    "when the type is JString and it can be converted into JDouble" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"score": "12"},
        {"score": "-34"},
        {"score": "0"},
        {"score": "12.34"},
        {"score": "-56.78"},
        {"score": "0.0"}
      ]
      """)

      "should never raise any error" in {
        errors shouldBe empty
      }
    }

    "when the type is neither JInt nor JDouble nor JString" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"score": ["12"]},
        {"score": [0]},
        {"score": [12.34]},
        {"score": false}
      ]
      """)

      "should raise TypeErrorException" in {
        errors should have size 4
        errors(0) shouldBe a [TypeErrorException]
        errors(0) should have (
          'depth        (Seq("[0]", "score")),
          'expectedType ("STR_REAL_RANGE(null,null)"),
        )
        errors(1) shouldBe a [TypeErrorException]
        errors(1) should have (
          'depth        (Seq("[1]", "score")),
          'expectedType ("STR_REAL_RANGE(null,null)"),
        )
        errors(2) shouldBe a [TypeErrorException]
        errors(2) should have (
          'depth        (Seq("[2]", "score")),
          'expectedType ("STR_REAL_RANGE(null,null)"),
        )
        errors(3) shouldBe a [TypeErrorException]
        errors(3) should have (
          'depth        (Seq("[3]", "score")),
          'expectedType ("STR_REAL_RANGE(null,null)"),
        )
      }
    }

    "when the type is JString but it cannot be converted into JDouble" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"score": ""},
        {"score": "hello"},
        {"score": "abc0"},
        {"score": "0abc"}
      ]
      """)

      "should raise TypeErrorException" in {
        errors should have size 4
        errors(0) shouldBe a [TypeErrorException]
        errors(0) should have (
          'depth        (Seq("[0]", "score")),
          'expectedType ("STR_REAL_RANGE(null,null)"),
        )
        errors(1) shouldBe a [TypeErrorException]
        errors(1) should have (
          'depth        (Seq("[1]", "score")),
          'expectedType ("STR_REAL_RANGE(null,null)"),
        )
        errors(2) shouldBe a [TypeErrorException]
        errors(2) should have (
          'depth        (Seq("[2]", "score")),
          'expectedType ("STR_REAL_RANGE(null,null)"),
        )
        errors(3) shouldBe a [TypeErrorException]
        errors(3) should have (
          'depth        (Seq("[3]", "score")),
          'expectedType ("STR_REAL_RANGE(null,null)"),
        )
      }
    }
  }

  "REAL_RANGE" - {
    val valueType = ARRAY(null, null, NULL(JsonSchema(
      REQ("a") -> NULL(REAL_RANGE(null, null)),
      REQ("b") -> NULL(REAL_RANGE(-1.0, null)),
      REQ("c") -> NULL(REAL_RANGE(null, 1.0)),
      REQ("d") -> NULL(REAL_RANGE(-0.5, 0.5))
    )))

    "when the range does not exceed the limit" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"a": -10000.0, "b": -1.0, "c": -10000, "d": 0},
        {"a": 10000.0, "b": 10000.0, "c": 1, "d": 0}
      ]
      """)

      "should never raise any error" in {
        errors shouldBe empty
      }
    }

    "when the range exceeds the lower limit" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"a": -10000.0, "b": -1.1, "c": -10000, "d": -1}
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
        {"a": 10000.0, "b": 10000.0, "c": 2, "d": 1}
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

    "when the type is neither JInt nor JDouble" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"a": "0", "b": [0], "c": [0.0], "d": false}
      ]
      """)

      "should raise TypeErrorException" in {
        errors should have size 4
        errors(0) shouldBe a [TypeErrorException]
        errors(0) should have (
          'depth        (Seq("[0]", "a")),
          'expectedType ("REAL_RANGE(null,null)"),
        )
        errors(1) shouldBe a [TypeErrorException]
        errors(1) should have (
          'depth        (Seq("[0]", "b")),
          'expectedType ("REAL_RANGE(-1.0,null)"),
        )
        errors(2) shouldBe a [TypeErrorException]
        errors(2) should have (
          'depth        (Seq("[0]", "c")),
          'expectedType ("REAL_RANGE(null,1.0)"),
        )
        errors(3) shouldBe a [TypeErrorException]
        errors(3) should have (
          'depth        (Seq("[0]", "d")),
          'expectedType ("REAL_RANGE(-0.5,0.5)"),
        )
      }
    }
  }

  "STR_INT_RANGE" - {
    val valueType = ARRAY(null, null, NULL(JsonSchema(
      REQ("a") -> NULL(STR_INT_RANGE(null, null)),
      REQ("b") -> NULL(STR_INT_RANGE(-10, null)),
      REQ("c") -> NULL(STR_INT_RANGE(null, 10)),
      REQ("d") -> NULL(STR_INT_RANGE(-5, 5))
    )))

    "when the range does not exceed the limit" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"a": -10000, "b": "-10", "c": "-10000", "d": -5},
        {"a": 10000, "b": "10000", "c": "10", "d": 5}
      ]
      """)

      "should never raise any error" in {
        errors shouldBe empty
      }
    }

    "when the range exceeds the lower limit" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"a": -10000, "b": "-11", "c": "-10000", "d": -6}
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
        {"a": 10000, "b": "10000", "c": "11", "d": 6}
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

    "when the type is neither JInt nor JString" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"a": ["0"], "b": [0], "c": 0.0, "d": false}
      ]
      """)

      "should raise TypeErrorException" in {
        errors should have size 4
        errors(0) shouldBe a [TypeErrorException]
        errors(0) should have (
          'depth        (Seq("[0]", "a")),
          'expectedType ("STR_INT_RANGE(null,null)"),
        )
        errors(1) shouldBe a [TypeErrorException]
        errors(1) should have (
          'depth        (Seq("[0]", "b")),
          'expectedType ("STR_INT_RANGE(-10,null)"),
        )
        errors(2) shouldBe a [TypeErrorException]
        errors(2) should have (
          'depth        (Seq("[0]", "c")),
          'expectedType ("STR_INT_RANGE(null,10)"),
        )
        errors(3) shouldBe a [TypeErrorException]
        errors(3) should have (
          'depth        (Seq("[0]", "d")),
          'expectedType ("STR_INT_RANGE(-5,5)"),
        )
      }
    }

    "when the type is JString but it cannot be converted into JInt" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"a": "", "b": "hello", "c": "1.0", "d": "0abc0"}
      ]
      """)

      "should raise TypeErrorException" in {
        errors should have size 4
        errors(0) shouldBe a [TypeErrorException]
        errors(0) should have (
          'depth        (Seq("[0]", "a")),
          'expectedType ("STR_INT_RANGE(null,null)"),
        )
        errors(1) shouldBe a [TypeErrorException]
        errors(1) should have (
          'depth        (Seq("[0]", "b")),
          'expectedType ("STR_INT_RANGE(-10,null)"),
        )
        errors(2) shouldBe a [TypeErrorException]
        errors(2) should have (
          'depth        (Seq("[0]", "c")),
          'expectedType ("STR_INT_RANGE(null,10)"),
        )
        errors(3) shouldBe a [TypeErrorException]
        errors(3) should have (
          'depth        (Seq("[0]", "d")),
          'expectedType ("STR_INT_RANGE(-5,5)"),
        )
      }
    }
  }

  "STR_REAL_RANGE" - {
    val valueType = ARRAY(null, null, NULL(JsonSchema(
      REQ("a") -> NULL(STR_REAL_RANGE(null, null)),
      REQ("b") -> NULL(STR_REAL_RANGE(-1.0, null)),
      REQ("c") -> NULL(STR_REAL_RANGE(null, 1.0)),
      REQ("d") -> NULL(STR_REAL_RANGE(-0.5, 0.5))
    )))

    "when the range does not exceed the limit" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"a": -10000.0, "b": "-1.0", "c": "-10000", "d": 0},
        {"a": 10000.0, "b": "10000.0", "c": "1", "d": 0}
      ]
      """)

      "should never raise any error" in {
        errors shouldBe empty
      }
    }

    "when the range exceeds the lower limit" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"a": -10000.0, "b": "-1.1", "c": "-10000", "d": -1}
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
        {"a": 10000.0, "b": "10000.0", "c": "2", "d": 1}
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

    "when the type is neither JInt nor JDouble nor JString" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"a": ["0"], "b": [0], "c": [0.0], "d": false}
      ]
      """)

      "should raise TypeErrorException" in {
        errors should have size 4
        errors(0) shouldBe a [TypeErrorException]
        errors(0) should have (
          'depth        (Seq("[0]", "a")),
          'expectedType ("STR_REAL_RANGE(null,null)"),
        )
        errors(1) shouldBe a [TypeErrorException]
        errors(1) should have (
          'depth        (Seq("[0]", "b")),
          'expectedType ("STR_REAL_RANGE(-1.0,null)"),
        )
        errors(2) shouldBe a [TypeErrorException]
        errors(2) should have (
          'depth        (Seq("[0]", "c")),
          'expectedType ("STR_REAL_RANGE(null,1.0)"),
        )
        errors(3) shouldBe a [TypeErrorException]
        errors(3) should have (
          'depth        (Seq("[0]", "d")),
          'expectedType ("STR_REAL_RANGE(-0.5,0.5)"),
        )
      }
    }

    "when the type is JString but it cannot be converted into JDouble" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"a": "", "b": "hello", "c": "abc0", "d": "0abc"}
      ]
      """)

      "should raise TypeErrorException" in {
        errors should have size 4
        errors(0) shouldBe a [TypeErrorException]
        errors(0) should have (
          'depth        (Seq("[0]", "a")),
          'expectedType ("STR_REAL_RANGE(null,null)"),
        )
        errors(1) shouldBe a [TypeErrorException]
        errors(1) should have (
          'depth        (Seq("[0]", "b")),
          'expectedType ("STR_REAL_RANGE(-1.0,null)"),
        )
        errors(2) shouldBe a [TypeErrorException]
        errors(2) should have (
          'depth        (Seq("[0]", "c")),
          'expectedType ("STR_REAL_RANGE(null,1.0)"),
        )
        errors(3) shouldBe a [TypeErrorException]
        errors(3) should have (
          'depth        (Seq("[0]", "d")),
          'expectedType ("STR_REAL_RANGE(-0.5,0.5)"),
        )
      }
    }
  }

  "STR_REGEX" - {
    val valueType = ARRAY(null, null, NULL(JsonSchema(
      REQ("date") -> NULL(STR_REGEX("""\d{4}-\d{2}-\d{2}""".r))
    )))

    "when the value matches the given regular expression" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"date": "1988-03-24"},
        {"date": "1234-56-78"},
        {"date": "0000-00-00"}
      ]
      """)

      "should never raise any error" in {
        errors shouldBe empty
      }
    }

    "when the value does not match the given regular expression" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"date": " 0000-00-00"},
        {"date": "0000-00-00 "},
        {"date": "hello"},
        {"date": "1234"},
        {"date": ""}
      ]
      """)

      "should raise TypeErrorException" in {
        errors should have size 5
        errors(0) shouldBe a [TypeErrorException]
        errors(0) should have (
          'depth        (Seq("[0]", "date")),
          'expectedType ("""STR_REGEX(\d{4}-\d{2}-\d{2})"""),
        )
        errors(1) shouldBe a [TypeErrorException]
        errors(1) should have (
          'depth        (Seq("[1]", "date")),
          'expectedType ("""STR_REGEX(\d{4}-\d{2}-\d{2})"""),
        )
        errors(2) shouldBe a [TypeErrorException]
        errors(2) should have (
          'depth        (Seq("[2]", "date")),
          'expectedType ("""STR_REGEX(\d{4}-\d{2}-\d{2})"""),
        )
        errors(3) shouldBe a [TypeErrorException]
        errors(3) should have (
          'depth        (Seq("[3]", "date")),
          'expectedType ("""STR_REGEX(\d{4}-\d{2}-\d{2})"""),
        )
        errors(4) shouldBe a [TypeErrorException]
        errors(4) should have (
          'depth        (Seq("[4]", "date")),
          'expectedType ("""STR_REGEX(\d{4}-\d{2}-\d{2})"""),
        )
      }
    }

    "when the type is not JString" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"date": [""]},
        {"date": 0},
        {"date": 0.0},
        {"date": false}
      ]
      """)

      "should raise TypeErrorException" in {
        errors should have size 4
        errors(0) shouldBe a [TypeErrorException]
        errors(0) should have (
          'depth        (Seq("[0]", "date")),
          'expectedType ("""STR_REGEX(\d{4}-\d{2}-\d{2})"""),
        )
        errors(1) shouldBe a [TypeErrorException]
        errors(1) should have (
          'depth        (Seq("[1]", "date")),
          'expectedType ("""STR_REGEX(\d{4}-\d{2}-\d{2})"""),
        )
        errors(2) shouldBe a [TypeErrorException]
        errors(2) should have (
          'depth        (Seq("[2]", "date")),
          'expectedType ("""STR_REGEX(\d{4}-\d{2}-\d{2})"""),
        )
        errors(3) shouldBe a [TypeErrorException]
        errors(3) should have (
          'depth        (Seq("[3]", "date")),
          'expectedType ("""STR_REGEX(\d{4}-\d{2}-\d{2})"""),
        )
      }
    }
  }

  "ARRAY" - {
    val valueType = ARRAY(null, null, NULL(JsonSchema(
      REQ("a") -> NULL(ARRAY(null, null, NULL(ANY))),
      REQ("b") -> NULL(ARRAY(1, null, NULL(ANY))),
      REQ("c") -> NULL(ARRAY(null, 3, NULL(ANY))),
      REQ("d") -> NULL(ARRAY(2, 2, NULL(ANY)))
    )))

    "when the type is JArray and the size does not exceed the limit" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"a": [], "b": [1], "c": [], "d": [1, 2]},
        {"a": [1, 2, 3], "b": [1, 2, 3], "c": [1, 2, 3], "d": [1, 2]}
      ]
      """)

      "should never raise any error" in {
        errors shouldBe empty
      }
    }

    "when the type is not JArray" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"a": "", "b": 0, "c": 0.0, "d": false}
      ]
      """)

      "should raise TypeErrorException" in {
        errors should have size 4
        errors(0) shouldBe a [TypeErrorException]
        errors(0) should have (
          'depth        (Seq("[0]", "a")),
          'expectedType ("ARRAY(null,null,NULL(ANY))"),
        )
        errors(1) shouldBe a [TypeErrorException]
        errors(1) should have (
          'depth        (Seq("[0]", "b")),
          'expectedType ("ARRAY(1,null,NULL(ANY))"),
        )
        errors(2) shouldBe a [TypeErrorException]
        errors(2) should have (
          'depth        (Seq("[0]", "c")),
          'expectedType ("ARRAY(null,3,NULL(ANY))"),
        )
        errors(3) shouldBe a [TypeErrorException]
        errors(3) should have (
          'depth        (Seq("[0]", "d")),
          'expectedType ("ARRAY(2,2,NULL(ANY))"),
        )
      }
    }

    "when the size exceeds the lower limit" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"a": [], "b": [], "c": [], "d": []}
      ]
      """)

      "should raise ArraySizeException" in {
        errors should have size 2
        errors(0) shouldBe a [ArraySizeException]
        errors(0) should have (
          'depth        (Seq("[0]", "b")),
          'expectedSize ("1~null"),
        )
        errors(1) shouldBe a [ArraySizeException]
        errors(1) should have (
          'depth        (Seq("[0]", "d")),
          'expectedSize ("2~2"),
        )
      }
    }

    "when the size exceeds the upper limit" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"a": [1, 2, 3, 4], "b": [1, 2, 3, 4], "c": [1, 2, 3, 4], "d": [1, 2, 3, 4]}
      ]
      """)

      "should raise ArraySizeException" in {
        errors should have size 2
        errors(0) shouldBe a [ArraySizeException]
        errors(0) should have (
          'depth        (Seq("[0]", "c")),
          'expectedSize ("null~3"),
        )
        errors(1) shouldBe a [ArraySizeException]
        errors(1) should have (
          'depth        (Seq("[0]", "d")),
          'expectedSize ("2~2"),
        )
      }
    }
  }

}

