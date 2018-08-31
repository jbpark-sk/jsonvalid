
package jbpark.jsonvalid

import jbpark.jsonvalid.JsonValidatorMeta._

import org.json4s.jackson.JsonMethods
import org.scalatest._


class JValueToolkitSpec extends FreeSpec with Matchers {

  "toStringSafe" - {
    val jValue = JsonMethods.parse("""
    {
      "str": {
        "hello": "world",
        "empty": ""
      },
      "int": {
        "one": 1,
        "zero": 0
      },
      "dbl": {
        "pi": 3.14,
        "zero": 0.0
      },
      "bool": {
        "true": true,
        "false": false
      },
      "null": null
    }
    """)

    "when the type is neither JNothing nor JNull" - {
      "should return the same result as toString" in {
        (jValue \ "str" \ "hello").toStringSafe  shouldBe "world"
        (jValue \ "str" \ "empty").toStringSafe  shouldBe ""
        (jValue \ "int" \ "one").toStringSafe    shouldBe "1"
        (jValue \ "int" \ "zero").toStringSafe   shouldBe "0"
        (jValue \ "dbl" \ "pi").toStringSafe     shouldBe "3.14"
        (jValue \ "dbl" \ "zero").toStringSafe   shouldBe "0.0"
        (jValue \ "bool" \ "true").toStringSafe  shouldBe "true"
        (jValue \ "bool" \ "false").toStringSafe shouldBe "false"
      }
    }

    "when the type is JNothing or JNull" - {
      "should return the empty string" in {
        (jValue \ "not_found").toStringSafe shouldBe ""
        (jValue \ "null").toStringSafe      shouldBe ""
      }
    }
  }

  "getOneOf" - {
    val jValue = JsonMethods.parse("""
    {
      "arr": [1, 2, 3],
      "empty_arr": [],
      "obj": {"foo": "bar"},
      "empty_obj": {},
      "null": null,
      "str": "hello"
    }
    """)

    "when the type is JArray and the value is not empty" - {
      "should select the key" in {
        jValue.getOneOf("arr", "str") shouldBe (jValue \ "arr")
      }
    }

    "when the type is JArray and the value is empty" - {
      "should not select the key" in {
        jValue.getOneOf("empty_arr", "str") shouldBe (jValue \ "str")
      }
    }

    "when the type is JObject and the value is not empty" - {
      "should select the key" in {
        jValue.getOneOf("obj", "str") shouldBe (jValue \ "obj")
      }
    }

    "when the type is JObject and the value is empty" - {
      "should not select the key" in {
        jValue.getOneOf("empty_obj", "str") shouldBe (jValue \ "str")
      }
    }

    "when the type is JNothing" - {
      "should not select the key" in {
        jValue.getOneOf("not_found", "str") shouldBe (jValue \ "str")
      }
    }

    "when the type is JNull" - {
      "should not select the key" in {
        jValue.getOneOf("null", "str") shouldBe (jValue \ "str")
      }
    }
  }

}

