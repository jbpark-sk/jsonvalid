
package jbpark.jsonvalid

import jbpark.jsonvalid.JsonValidatorMeta._

import org.scalatest._


class KeyConstraintSpec extends FreeSpec with Matchers {

  "REQ, REQ_ALL" - {
    val valueType = JsonSchema(
      REQ("hello") -> NULL(ANY),
      REQ("감자", "고구마") -> NULL(JsonSchema(
        REQ_ALL("A", "B", "C") -> NULL(ANY)
      )),
    )

    "when all of the given keys exist" - {
      val errors = JsonValidator(valueType, true).check("""
      {
        "hello": 0,
        "고구마": {"A": 1, "B": 2, "C": 3},
        "감자": {"A": null, "B": 0, "C": ""}
      }
      """)

      "should never raise any error" in {
        errors shouldBe empty
      }
    }

    "when some of the given keys exist" - {
      val errors = JsonValidator(valueType, true).check("""
      {
        "감자": {"A": null}
      }
      """)

      "should raise KeysNotFoundException" in {
        errors should have size 3
        errors(0) shouldBe a [KeysNotFoundException]
        errors(0) should have (
          'depth        (Seq()),
          'expectedKeys (Seq("hello")),
          'actualKeys   (Seq()),
        )
        errors(1) shouldBe a [KeysNotFoundException]
        errors(1) should have (
          'depth        (Seq()),
          'expectedKeys (Seq("감자", "고구마")),
          'actualKeys   (Seq("감자")),
        )
        errors(2) shouldBe a [KeysNotFoundException]
        errors(2) should have (
          'depth        (Seq("감자")),
          'expectedKeys (Seq("A", "B", "C")),
          'actualKeys   (Seq("A")),
        )
      }
    }
  }

  "OPT" - {
    val valueType = JsonSchema(
      OPT("hello") -> NULL(ANY),
      OPT("감자", "고구마") -> NULL(JsonSchema(
        OPT("A", "B", "C") -> NULL(ANY)
      )),
    )

    "when none of the given keys exists" - {
      val errors = JsonValidator(valueType, true).check("""
      {
        "감자": {"A": null}
      }
      """)

      "should never raise any error" in {
        errors shouldBe empty
      }
    }
  }

  "REQ_AT_LEAST_ONE" - {
    val valueType = ARRAY(null, null, NULL(JsonSchema(
      REQ_AT_LEAST_ONE("1", "2", "3", "4", "5") -> NULL(ANY)
    )))

    "when some of the given keys exist" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"0": 0, "1": 1, "2": null},
        {"1": 1, "2": 2, "3": 3},
        {"1": null, "2": null, "3": null, "4": null, "5": null},
        {"0": null, "1": null, "2": null, "3": null, "4": null, "5": null}
      ]
      """)

      "should never raise any error" in {
        errors shouldBe empty
      }
    }

    "when there is only one of the given keys" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"0": 0, "1": 1},
        {"2": null},
        {"3": 3},
        {"4": ""},
        {"5": "5"}
      ]
      """)

      "should never raise any error" in {
        errors shouldBe empty
      }
    }

    "when none of the given keys exists" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {},
        {"0": null},
        {"6": null, "7": null},
        {"5": null, "6": null}
      ]
      """)

      "should raise KeysNotFoundException" in {
        errors should have size 3
        errors(0) shouldBe a [KeysNotFoundException]
        errors(0) should have (
          'depth        (Seq("[0]")),
          'expectedKeys (Seq("1", "2", "3", "4", "5")),
          'actualKeys   (Seq()),
        )
        errors(1) shouldBe a [KeysNotFoundException]
        errors(1) should have (
          'depth        (Seq("[1]")),
          'expectedKeys (Seq("1", "2", "3", "4", "5")),
          'actualKeys   (Seq()),
        )
        errors(2) shouldBe a [KeysNotFoundException]
        errors(2) should have (
          'depth        (Seq("[2]")),
          'expectedKeys (Seq("1", "2", "3", "4", "5")),
          'actualKeys   (Seq()),
        )
      }
    }
  }

  "REQ_ONLY_ONE" - {
    val valueType = ARRAY(null, null, NULL(JsonSchema(
      REQ_ONLY_ONE("1", "2", "3") -> NULL(ANY)
    )))

    "when there is only one of the given keys" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"0": 0, "1": 1},
        {"2": 2},
        {"3": 3, "4": 4, "5": 5}
      ]
      """)

      "should never raise any error" in {
        errors shouldBe empty
      }
    }

    "when none of the given keys exists" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"0": 0},
        {},
        {"5": 5}
      ]
      """)

      "should raise KeysNotFoundException" in {
        errors should have size 3
        errors(0) shouldBe a [KeysNotFoundException]
        errors(0) should have (
          'depth        (Seq("[0]")),
          'expectedKeys (Seq("1", "2", "3")),
          'actualKeys   (Seq()),
        )
        errors(1) shouldBe a [KeysNotFoundException]
        errors(1) should have (
          'depth        (Seq("[1]")),
          'expectedKeys (Seq("1", "2", "3")),
          'actualKeys   (Seq()),
        )
        errors(2) shouldBe a [KeysNotFoundException]
        errors(2) should have (
          'depth        (Seq("[2]")),
          'expectedKeys (Seq("1", "2", "3")),
          'actualKeys   (Seq()),
        )
      }
    }

    "when there are two or more of the given keys" - {
      val errors = JsonValidator(valueType, true).check("""
      [
        {"0": 0, "1": 1},
        {"2": 2, "3": 3},
        {"1": 1, "2": 2, "3": 3}
      ]
      """)

      "should raise RedundantKeyException" in {
        errors should have size 2
        errors(0) shouldBe a [RedundantKeyException]
        errors(0) should have (
          'depth        (Seq("[1]")),
          'expectedKeys (Seq("1", "2", "3")),
          'actualKeys   (Seq("2", "3")),
        )
        errors(1) shouldBe a [RedundantKeyException]
        errors(1) should have (
          'depth        (Seq("[2]")),
          'expectedKeys (Seq("1", "2", "3")),
          'actualKeys   (Seq("1", "2", "3")),
        )
      }
    }
  }

}

