
package jbpark.jsonvalid

import org.json4s._

import scala.util.matching.Regex


/**
 * JsonValidator에서 사용하는 클래스나 헬퍼들을 정의한다.
 * Transformer 구현 시 거의 필수적으로 사용된다.
 */
object JsonValidatorMeta {

  /**********************/
  /* 1. Key Constraints */
  /**********************/

  /**
   * 키의 필수 혹은 선택 여부를 지정할 때 사용한다.
   * 조건에 어긋나면 KeysNotFoundException, RedundantKeyException이 발생한다.
   *
   * REQ             : Required. 나열된 모든 키는 반드시 존재해야 한다.
   * OPT             : Optional. 나열된 모든 키는 존재하지 않아도 된다.
   * REQ_AT_LEAST_ONE: 나열된 키 중에서 적어도 하나 이상 존재해야 한다.
   * REQ_ONLY_ONE    : 나열된 키 목록 가운데 오직 하나만 존재해야 한다.
   * REQ_ALL         : REQ와 동일
   */

  /***********************/
  /* 2. Null Constraints */
  /***********************/

  /**
   * Null 값의 허용 여부를 지정할 때 사용한다.
   * 조건에 어긋나면 NullConstraintException이 발생한다.
   *
   * NULL    : null 값을 허용한다.
   * NOT_NULL: null 값을 허용하지 않는다.
   */

  /******************/
  /* 3. Value Types */
  /******************/

  /**
   * 값의 타입이나 조건 등을 지정할 때 사용한다.
   * 만약 값의 타입이 일치하지 않을 경우 TypeErrorException이 발생한다.
   */

  /**************************************/
  /* 3-1. Value Types - Primitive Types */
  /**************************************/

  /**
   * 기본적인 형태로 값의 타입 체크만 하고자 할 때 사용한다.
   *
   * STR : String. 문자열 타입     (예시: "abc", "123")
   * INT : Integer. 정수 타입      (예시: 123, 456)
   * DBL : Double. 부동소수점 타입 (예시: 12.34, 5.67)
   * BOOL: Boolean. 참 거짓 타입   (예시: true, false)
   * ANY : 타입 체크를 수행하지 않는다.
   *
   * STR_LEN:
   * 길이 제약 조건을 추가로 지정할 수 있는 문자열 타입이다.
   * 길이 제한이 없을 경우 null로 지정하면 된다.
   * 길이 제약 조건에 어긋나면 StringLengthException이 발생한다.
   *
   * INT_RANGE, DBL_RANGE:
   * 범위 제약 조건을 추가로 지정할 수 있는 정수, 부동소수점 타입이다.
   * 범위 제한이 없을 경우 null로 지정하면 된다.
   * 범위 제약 조건에 어긋나면 NumberRangeException이 발생한다.
   */

  /************************************/
  /* 3-2. Value Types - Complex Types */
  /************************************/

  /**
   * 복합적인 형태의 타입을 체크하고자 할 때 사용한다.
   *
   * STR_INT : 문자열을 포함한 모든 정수 타입 (예시: "123", 456)
   * STR_REAL: 문자열을 포함한 모든 실수 타입 (예시: "12.34", 1, 1.0)
   *
   * STR_INT_RANGE, STR_REAL_RANGE:
   * 각각 STR_INT, STR_REAL과 동일하며 범위 제약 조건을 추가로 지정할 수 있는 타입이다.
   * 범위 제한이 없을 경우 null로 지정하면 된다.
   * 범위 제약 조건에 어긋나면 NumberRangeException이 발생한다.
   *
   * STR_REGEX: 정규표현식 제약 조건을 추가로 지정할 수 있는 문자열 타입
   *
   * ARRAY:
   * 배열 타입. (예시: [1, 2], ["ab", "cd", "ef"])
   * 배열의 크기 제약 조건을 추가로 지정할 수 있다. 크기 제한이 없을 경우 null로 지정하면 된다.
   * 크기 제약 조건에 어긋나면 ArraySizeException이 발생한다.
   */

  /*****************************************/
  /* 3-3. Value Types - Domain Constraints */
  /*****************************************/

  /**
   * 값의 도메인이 정해져 있을 때 사용한다.
   * 조건에 어긋나면 DomainConstraintException이 발생한다.
   *
   * ALLOW_VAL: 나열된 값만을 허용한다.
   * Y_OR_N   : ALLOW_VAL("Y", "N")과 동일
   * ANY_BOOL : ALLOW_VAL("1", "0", 1, 0, true, false)와 동일
   */

  /********************************/
  /* 3-4. Value Types - Validator */
  /********************************/

  /**
   * Validator(람다식)로 값을 체크하고자 할 때 사용한다.
   * Validator가 false를 리턴하면 ValidationFailedException이 발생한다.
   *
   * STR_CHECK : String. 문자열 타입의 Validator
   * INT_CHECK : Integer. 정수 타입의 Validator
   * DBL_CHECK : Double. 부동소수점 타입의 Validator
   * BOOL_CHECK: Boolean. 참 거짓 타입의 Validator
   * ANY_CHECK : JValue 타입의 Validator
   */

  /*********************************/
  /* 3-5. Value Types - JsonSchema */
  /*********************************/

  /**
   * JSON Schema를 정의할 때 사용한다.
   * JSON Schema는 { ... } 형태의 객체 타입을 표현하며 일반적인 사용법은 다음과 같다.
   *
   * JsonSchema(
   *   KeyConstraint(...) -> NullConstraint(ValueType),
   *   ...
   * )
   */

  /*********************/
  /* 4. JValue Toolkit */
  /*********************/

  /**
   * JValue 형태로 된 JSON을 편리하게 조회할 수 있도록 도와주는 각종 헬퍼 모음이다.
   *
   * toStringSafe: JValue가 감싸고 있는 값을 String으로 바로 변환한다.
   * getOneOf    : 나열된 키 가운데 값이 존재하는 하나를 선택한다.
   */


  /**
   * JsonValidator에서 발생하는 모든 예외는 JsonValidatorException으로부터 파생된다.
   */
  sealed abstract class JsonValidatorException extends Exception {
    def depth: Seq[String]
  }


  /**********************/
  /* 1. Key Constraints */
  /**********************/

  sealed trait KeyConstraint {
    def keys: Seq[String]
  }

  sealed case class REQ(keys: String*) extends KeyConstraint
  sealed case class OPT(keys: String*) extends KeyConstraint
  sealed case class REQ_AT_LEAST_ONE(keys: String*) extends KeyConstraint
  sealed case class REQ_ONLY_ONE(keys: String*) extends KeyConstraint
  sealed case class REQ_ALL(keys: String*) extends KeyConstraint

  /**
   * Required로 지정된 키가 존재하지 않을 경우 발생한다.
   *
   * @param depth JSON에서의 위치
   * @param expectedKeys 지정된 키의 목록
   * @param actualKeys 실제 존재하는 키의 목록
   */
  sealed case class KeysNotFoundException(
    depth: Seq[String],
    expectedKeys: Seq[String],
    actualKeys: Seq[String]
  ) extends JsonValidatorException

  /**
   * REQ_ONLY_ONE으로 지정된 키가 두 개 이상 존재할 경우 발생한다.
   *
   * @param depth JSON에서의 위치
   * @param expectedKeys 지정된 키의 목록
   * @param actualKeys 실제 존재하는 키의 목록
   */
  sealed case class RedundantKeyException(
    depth: Seq[String],
    expectedKeys: Seq[String],
    actualKeys: Seq[String]
  ) extends JsonValidatorException


  /***********************/
  /* 2. Null Constraints */
  /***********************/

  sealed trait NullConstraint {
    def valueType: ValueType
  }

  sealed case class NULL(valueType: ValueType) extends NullConstraint
  sealed case class NOT_NULL(valueType: ValueType) extends NullConstraint

  /**
   * NOT_NULL로 지정된 키의 값이 null일 경우 발생한다.
   *
   * @param depth JSON에서의 위치
   */
  sealed case class NullConstraintException(
    depth: Seq[String]
  ) extends JsonValidatorException


  /******************/
  /* 3. Value Types */
  /******************/

  sealed trait ValueType

  /**
   * 값의 타입이 일치하지 않을 경우 발생한다.
   *
   * @param depth JSON에서의 위치
   * @param expectedType 기대했던 타입
   * @param jValue 실제 값
   */
  sealed case class TypeErrorException(
    depth: Seq[String],
    expectedType: String,
    jValue: JValue
  ) extends JsonValidatorException


  /**************************************/
  /* 3-1. Value Types - Primitive Types */
  /**************************************/

  sealed trait PrimitiveType extends ValueType

  case object STR extends PrimitiveType
  case object INT extends PrimitiveType
  case object DBL extends PrimitiveType
  case object BOOL extends PrimitiveType
  case object ANY extends PrimitiveType

  sealed case class STR_LEN(
    min: java.lang.Integer,
    max: java.lang.Integer
  ) extends PrimitiveType

  sealed case class INT_RANGE(
    min: java.lang.Long,
    max: java.lang.Long
  ) extends PrimitiveType

  sealed case class DBL_RANGE(
    min: java.lang.Double,
    max: java.lang.Double
  ) extends PrimitiveType

  /**
   * 문자열의 길이가 알맞지 않을 경우 발생한다.
   *
   * @param depth JSON에서의 위치
   * @param expectedLength 기대했던 길이
   * @param jString 실제 값
   */
  sealed case class StringLengthException(
    depth: Seq[String],
    expectedLength: String,
    jString: JString
  ) extends JsonValidatorException

  /**
   * 숫자의 범위가 알맞지 않을 경우 발생한다.
   *
   * @param depth JSON에서의 위치
   * @param expectedRange 기대했던 범위
   * @param jValue 실제 값
   */
  sealed case class NumberRangeException(
    depth: Seq[String],
    expectedRange: String,
    jValue: JValue
  ) extends JsonValidatorException


  /************************************/
  /* 3-2. Value Types - Complex Types */
  /************************************/

  sealed trait ComplexType extends ValueType

  case object STR_INT extends ComplexType
  case object STR_REAL extends ComplexType

  sealed case class STR_INT_RANGE(
    min: java.lang.Long,
    max: java.lang.Long
  ) extends ComplexType

  sealed case class STR_REAL_RANGE(
    min: java.lang.Double,
    max: java.lang.Double
  ) extends ComplexType

  sealed case class STR_REGEX(
    regex: Regex
  ) extends ComplexType

  sealed case class ARRAY(
    min: java.lang.Integer,
    max: java.lang.Integer,
    subNullConstraint: NullConstraint
  ) extends ComplexType

  /**
   * 배열의 크기가 알맞지 않을 경우 발생한다.
   *
   * @param depth JSON에서의 위치
   * @param expectedSize 기대했던 크기
   * @param jArray 실제 값
   */
  sealed case class ArraySizeException(
    depth: Seq[String],
    expectedSize: String,
    jArray: JArray
  ) extends JsonValidatorException


  /*****************************************/
  /* 3-3. Value Types - Domain Constraints */
  /*****************************************/

  sealed trait DomainConstraint extends ValueType

  sealed case class ALLOW_VAL(values: Any*) extends DomainConstraint

  case object Y_OR_N extends DomainConstraint
  case object ANY_BOOL extends DomainConstraint

  /**
   * ALLOW_VAL에 나열된 값이 아닐 경우 발생한다.
   *
   * @param depth JSON에서의 위치
   * @param expectedValues 기대했던 값
   * @param jValue 실제 값
   */
  sealed case class DomainConstraintException(
    depth: Seq[String],
    expectedValues: Seq[Any],
    jValue: JValue
  ) extends JsonValidatorException


  /********************************/
  /* 3-4. Value Types - Validator */
  /********************************/

  sealed trait Validator extends ValueType

  sealed case class STR_CHECK(validator: String => Boolean) extends Validator
  sealed case class INT_CHECK(validator: BigInt => Boolean) extends Validator
  sealed case class DBL_CHECK(validator: Double => Boolean) extends Validator
  sealed case class BOOL_CHECK(validator: Boolean => Boolean) extends Validator
  sealed case class ANY_CHECK(validator: JValue => Boolean) extends Validator

  /**
   * Validator가 false를 리턴할 경우 발생한다.
   *
   * @param depth JSON에서의 위치
   * @param jValue 실제 값
   */
  sealed case class ValidationFailedException(
    depth: Seq[String],
    jValue: JValue
  ) extends JsonValidatorException


  /*********************************/
  /* 3-5. Value Types - JsonSchema */
  /*********************************/

  sealed case class JsonSchema(
    rules: (KeyConstraint, NullConstraint)*
  ) extends ValueType


  /*********************/
  /* 4. JValue Toolkit */
  /*********************/

  sealed implicit class JValueToolkit(jValue: JValue) {
    def toStringSafe = jValue match {
      case JNothing => ""
      case JNull    => ""
      case jValue   => jValue.values.toString
    }

    def getOneOf(keys: String*) = keys.map(jValue \ _).find {
      case JArray(values: List[JValue])             => values.nonEmpty
      case JObject(values: List[(String, JValue)])  => values.nonEmpty
      case jValue                                   => jValue != JNothing && jValue != JNull
    }.get
  }

}

