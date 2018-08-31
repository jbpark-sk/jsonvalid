
package jbpark.jsonvalid

import jbpark.jsonvalid.JsonValidatorMeta._

import org.json4s._
import org.json4s.jackson.JsonMethods

import scala.util.matching.Regex


/**
 * JsonValidator 팩토리
 */
object JsonValidator {

  def apply(
    valueType: ValueType,
    collectMultipleErrors: Boolean = false
  ): JsonValidator = new JsonValidator(valueType, collectMultipleErrors)


  /**
   * JsonValidatorException을 알아보기 편한 문자열 형태로 변환한다.
   *
   * @param errors 발생한 예외들
   * @param jsonString JSON이 담긴 문자열
   * @return 변환된 문자열 형태
   */
  def dumpErrors(
    errors: Seq[_ <: Exception],
    jsonString: String
  ): Seq[String] = errors.map { error => (error, error.getClass.getSimpleName) }.map {

    case (error: KeysNotFoundException, name: String) =>
      val expected = error.expectedKeys.mkString("[", ",", "]")
      val actual = error.actualKeys.mkString("[", ",", "]")
      s"$name (depth=${error.depth.mkString(".")} expected=$expected actual=$actual)"

    case (error: RedundantKeyException, name: String) =>
      val expected = error.expectedKeys.mkString("[", ",", "]")
      val actual = error.actualKeys.mkString("[", ",", "]")
      s"$name (depth=${error.depth.mkString(".")} expected=$expected actual=$actual)"

    case (error: NullConstraintException, name: String) =>
      s"$name (key=${error.depth.mkString(".")})"

    case (error: TypeErrorException, name: String) =>
      s"$name (key=${error.depth.mkString(".")} expected=${error.expectedType} value=${error.jValue})"

    case (error: StringLengthException, name: String) =>
      val actual = error.jString.values.length
      s"$name (key=${error.depth.mkString(".")} expected=${error.expectedLength} actual=$actual value=${error.jString})"

    case (error: NumberRangeException, name: String) =>
      s"$name (key=${error.depth.mkString(".")} expected=${error.expectedRange} value=${error.jValue})"

    case (error: ArraySizeException, name: String) =>
      val actual = error.jArray.values.size
      s"$name (key=${error.depth.mkString(".")} expected=${error.expectedSize} actual=$actual)"

    case (error: DomainConstraintException, name: String) =>
      val expected = error.expectedValues.map { value =>
        val typeName = value.getClass.getSimpleName
        s"$typeName($value)"
      }.mkString("[", ",", "]")
      s"$name (key=${error.depth.mkString(".")} expected=$expected value=${error.jValue})"

    case (error: ValidationFailedException, name: String) =>
      s"$name (key=${error.depth.mkString(".")} value=${error.jValue})"

    case (error: Exception, name: String) =>
      s"$name"

  }.map { errorStr => s"$errorStr @ $jsonString" }

}


/**
 * JsonValidator 클래스
 *
 * Thread-safe 하므로 한 번 생성하면 병렬 처리 환경에서 문제 없이 재사용 가능하다.
 * 다만, Validator(람다식)의 thread-safety는 보장하지 않는다.
 *
 * @param valueType 유효성 검사의 기준이 되는 ValueType
 * @param collectMultipleErrors 여러 개의 문제점을 검출할 지의 여부
 */
class JsonValidator(valueType: ValueType, collectMultipleErrors: Boolean) {

  /**
   * 문제 없음을 의미하는 상수
   */
  protected final val pass: Seq[JsonValidatorException] = Seq()


  /**
   * 발생한 예외를 적절하게 처리한다.
   *
   * collectMultipleErrors의 값에 따라 처리하는 방법이 달라진다.
   * 해당 값이 true일 경우에는 최대한 많은 문제점을 찾아내려고 노력하지만,
   * false일 경우에는 발생한 예외를 던져서 유효성 검사를 즉시 종료시킨다.
   *
   * 구체적인 오류 보고를 원할 경우에는 true가 적절하며,
   * 수행 시간을 극대화하기 위해서는 false가 적절하다.
   *
   * @param error 발생한 예외
   * @return 발생한 예외
   */
  protected final def raiseError(
    error: JsonValidatorException
  ): Seq[JsonValidatorException] = {

    if (collectMultipleErrors)
      Seq(error)
    else
      throw error

  }


  /**
   * JsonSchema를 기반으로 주어진 JObject에 대해 유효성 검사를 수행하여 문제점들을 찾아낸다.
   *
   * JsonSchema에 정의된 각각의 룰에 대해서 유효성 검사를 시작한다.
   * 유효성 검사는 다음과 같은 순서로 이루어진다.
   *
   * 1. KeyConstraint 확인
   * 2. NullConstraint 확인
   * 3. ValueType 확인
   *
   * @param jsonSchema 유효성 검사를 수행하기 위해 참조하는 JsonSchema
   * @param jObject 유효성 검사의 대상이 되는 JObject
   * @param depth 현재 탐색 중인 JSON의 위치
   * @return 발생한 예외들
   */
  protected final def checkJsonSchema(
    jsonSchema: JsonSchema,
    jObject: JObject,
    depth: Seq[String]
  ): Seq[JsonValidatorException] = jsonSchema.rules.flatMap {

    case (keyConstraint, nullConstraint) =>
      val expectedKeys: Seq[String] = keyConstraint.keys
      val actualKeys: Seq[String] = expectedKeys.filter(key => (jObject \ key) != JNothing)

      val errors: Seq[JsonValidatorException] = keyConstraint match {
        // REQ: Required. 나열된 모든 키는 반드시 존재해야 한다.
        case REQ(_*) =>
          if (actualKeys.size == expectedKeys.size)
            pass
          else
            raiseError(KeysNotFoundException(depth, expectedKeys, actualKeys))

        // OPT: Optional. 나열된 모든 키는 존재하지 않아도 된다.
        case OPT(_*) =>
          pass

        // REQ_AT_LEAST_ONE: 나열된 키 중에서 적어도 하나 이상 존재해야 한다.
        case REQ_AT_LEAST_ONE(_*) =>
          if (actualKeys.size > 0)
            pass
          else
            raiseError(KeysNotFoundException(depth, expectedKeys, actualKeys))

        // REQ_ONLY_ONE: 나열된 키 목록 가운데 오직 하나만 존재해야 한다.
        case REQ_ONLY_ONE(_*) =>
          if (actualKeys.size == 1)
            pass
          else if (actualKeys.size == 0)
            raiseError(KeysNotFoundException(depth, expectedKeys, actualKeys))
          else
            raiseError(RedundantKeyException(depth, expectedKeys, actualKeys))

        // REQ_ALL: REQ와 동일
        case REQ_ALL(_*) =>
          if (actualKeys.size == expectedKeys.size)
            pass
          else
            raiseError(KeysNotFoundException(depth, expectedKeys, actualKeys))
      }

      errors ++ actualKeys.flatMap { (key: String) =>
        val jValue: JValue = (jObject \ key)
        val nextDepth: Seq[String] = (depth :+ key)

        checkNullConstraint(nullConstraint, jValue, nextDepth)
      }

  }


  /**
   * 주어진 JValue를 가지고 NullConstraint에 알맞은 유효성 검사를 수행한다.
   *
   * @param nullConstraint 유효성 검사의 기준이 되는 NullConstraint
   * @param jValue 유효성 검사의 대상이 되는 JValue
   * @param depth 현재 탐색 중인 JSON의 위치
   * @return 발생한 예외들
   */
  protected final def checkNullConstraint(
    nullConstraint: NullConstraint,
    jValue: JValue,
    depth: Seq[String]
  ): Seq[JsonValidatorException] = jValue match {

    case JNull =>
      nullConstraint match {
        case NULL(_) => pass
        case NOT_NULL(_) => raiseError(NullConstraintException(depth))
      }

    case _ =>
      checkValueType(nullConstraint.valueType, jValue, depth)

  }


  /**
   * 주어진 JValue를 가지고 ValueType에 알맞은 유효성 검사를 수행한다.
   *
   * @param valueType 유효성 검사의 기준이 되는 ValueType
   * @param jValue 유효성 검사의 대상이 되는 JValue
   * @param depth 현재 탐색 중인 JSON의 위치
   * @return 발생한 예외들
   */
  protected final def checkValueType(
    valueType: ValueType,
    jValue: JValue,
    depth: Seq[String]
  ): Seq[JsonValidatorException] = valueType match {

    // String. 문자열 타입
    case STR =>
      jValue match {
        case JString(_) => pass
        case _ => raiseError(TypeErrorException(depth, valueType.toString, jValue))
      }

    // Integer. 정수 타입
    case INT =>
      jValue match {
        case JInt(_) => pass
        case _ => raiseError(TypeErrorException(depth, valueType.toString, jValue))
      }

    // Double. 부동소수점 타입
    case DBL =>
      jValue match {
        case JDouble(_) => pass
        case _ => raiseError(TypeErrorException(depth, valueType.toString, jValue))
      }

    // Boolean. 참 거짓 타입
    case BOOL =>
      jValue match {
        case JBool(_) => pass
        case _ => raiseError(TypeErrorException(depth, valueType.toString, jValue))
      }

    // 타입 체크를 수행하지 않는다.
    case ANY =>
      pass

    // 길이 제약 조건을 추가로 지정할 수 있는 문자열 타입
    case STR_LEN(min, max) =>
      jValue match {
        case JString(value: String) =>
          if ((min == null || value.length >= min) && (max == null || value.length <= max))
            pass
          else
            raiseError(StringLengthException(depth, s"$min~$max", jValue.asInstanceOf[JString]))
        case _ =>
          raiseError(TypeErrorException(depth, valueType.toString, jValue))
      }

    // 범위 제약 조건을 추가로 지정할 수 있는 정수 타입
    case INT_RANGE(min, max) =>
      jValue match {
        case JInt(value: BigInt) =>
          if ((min == null || value >= BigInt(min)) && (max == null || value <= BigInt(max)))
            pass
          else
            raiseError(NumberRangeException(depth, s"$min~$max", jValue))
        case _ =>
          raiseError(TypeErrorException(depth, valueType.toString, jValue))
      }

    // 범위 제약 조건을 추가로 지정할 수 있는 부동소수점 타입
    case DBL_RANGE(min, max) =>
      jValue match {
        case JDouble(value: Double) =>
          if ((min == null || value >= min) && (max == null || value <= max))
            pass
          else
            raiseError(NumberRangeException(depth, s"$min~$max", jValue))
        case _ =>
          raiseError(TypeErrorException(depth, valueType.toString, jValue))
      }

    // 문자열을 포함한 모든 정수 타입
    case STR_INT =>
      checkValueType(STR_INT_RANGE(null, null), jValue, depth)

    // 문자열을 포함한 모든 실수 타입
    case STR_REAL =>
      checkValueType(STR_REAL_RANGE(null, null), jValue, depth)

    // STR_INT와 동일하며 범위 제약 조건을 추가로 지정할 수 있는 타입
    case STR_INT_RANGE(min, max) =>
      jValue match {
        case JString(value: String) =>
          try {
            checkValueType(INT_RANGE(min, max), JInt(BigInt(value)), depth)
          } catch {
            case _: NumberFormatException =>
              raiseError(TypeErrorException(depth, valueType.toString, jValue))
            case error: Exception =>
              throw error
          }
        case JInt(_) =>
          checkValueType(INT_RANGE(min, max), jValue, depth)
        case _ =>
          raiseError(TypeErrorException(depth, valueType.toString, jValue))
      }

    // STR_REAL과 동일하며 범위 제약 조건을 추가로 지정할 수 있는 타입
    case STR_REAL_RANGE(min, max) =>
      jValue match {
        case JString(value: String) =>
          try {
            checkValueType(DBL_RANGE(min, max), JDouble(value.toDouble), depth)
          } catch {
            case _: NumberFormatException =>
              raiseError(TypeErrorException(depth, valueType.toString, jValue))
            case error: Exception =>
              throw error
          }
        case JInt(value: BigInt) =>
          checkValueType(DBL_RANGE(min, max), JDouble(value.toDouble), depth)
        case JDouble(_) =>
          checkValueType(DBL_RANGE(min, max), jValue, depth)
        case _ =>
          raiseError(TypeErrorException(depth, valueType.toString, jValue))
      }

    // 정규표현식 제약 조건을 추가로 지정할 수 있는 문자열 타입
    case STR_REGEX(regex: Regex) =>
      jValue match {
        case JString(value: String) =>
          if (regex.unapplySeq(value).isDefined)
            pass
          else
            raiseError(TypeErrorException(depth, valueType.toString, jValue))
        case _ =>
          raiseError(TypeErrorException(depth, valueType.toString, jValue))
      }

    // 배열 타입
    case ARRAY(min, max, subNullConstraint) =>
      jValue match {
        case JArray(value: List[JValue]) =>
          val key: String = if (depth.isEmpty) "" else depth.last
          val depthExceptLast: Seq[String] = depth.dropRight(1)

          if ((min == null || value.size >= min) && (max == null || value.size <= max))
            value.view.zipWithIndex.flatMap { case (subJValue: JValue, index: Int) =>
              val keyWithIndex: String = s"$key[$index]"
              checkNullConstraint(subNullConstraint, subJValue, depthExceptLast :+ keyWithIndex)
            }.toSeq
          else
            raiseError(ArraySizeException(depth, s"$min~$max", jValue.asInstanceOf[JArray]))
        case _ =>
          raiseError(TypeErrorException(depth, valueType.toString, jValue))
      }

    // 나열된 값만을 허용
    case ALLOW_VAL(values @ _*) =>
      if (values.contains(jValue.values))
        pass
      else
        raiseError(DomainConstraintException(depth, values, jValue))

    case Y_OR_N =>
      checkValueType(ALLOW_VAL("Y", "N"), jValue, depth)

    case ANY_BOOL =>
      checkValueType(ALLOW_VAL("1", "0", 1, 0, true, false), jValue, depth)

    // String. 문자열 타입의 Validator
    case STR_CHECK(validator: (String => Boolean)) =>
      jValue match {
        case JString(value: String) =>
          if (validator(value))
            pass
          else
            raiseError(ValidationFailedException(depth, jValue))
        case _ =>
          raiseError(TypeErrorException(depth, valueType.toString.takeWhile(_ != '('), jValue))
      }

    // Integer. 정수 타입의 Validator
    case INT_CHECK(validator: (BigInt => Boolean)) =>
      jValue match {
        case JInt(value: BigInt) =>
          if (validator(value))
            pass
          else
            raiseError(ValidationFailedException(depth, jValue))
        case _ =>
          raiseError(TypeErrorException(depth, valueType.toString.takeWhile(_ != '('), jValue))
      }

    // Double. 부동소수점 타입의 Validator
    case DBL_CHECK(validator: (Double => Boolean)) =>
      jValue match {
        case JDouble(value: Double) =>
          if (validator(value))
            pass
          else
            raiseError(ValidationFailedException(depth, jValue))
        case _ =>
          raiseError(TypeErrorException(depth, valueType.toString.takeWhile(_ != '('), jValue))
      }

    // Boolean. 참 거짓 타입의 Validator
    case BOOL_CHECK(validator: (Boolean => Boolean)) =>
      jValue match {
        case JBool(value: Boolean) =>
          if (validator(value))
            pass
          else
            raiseError(ValidationFailedException(depth, jValue))
        case _ =>
          raiseError(TypeErrorException(depth, valueType.toString.takeWhile(_ != '('), jValue))
      }

    // JValue 타입의 Validator
    case ANY_CHECK(validator: (JValue => Boolean)) =>
      if (validator(jValue))
        pass
      else
        raiseError(ValidationFailedException(depth, jValue))

    // JsonSchema
    case jsonSchema: JsonSchema =>
      jValue match {
        case jObject: JObject =>
          checkJsonSchema(jsonSchema, jObject, depth)
        case _ =>
          raiseError(TypeErrorException(depth, valueType.toString.takeWhile(_ != '('), jValue))
      }

  }


  /**
   * 주어진 JSON 문자열을 파싱하고 유효성 검사를 수행한다.
   *
   * @param jsonString 파싱할 JSON이 담긴 문자열
   * @return 발생한 예외들
   */
  def check(jsonString: String): Seq[_ <: Exception] = try {
    // 파싱할 JSON이 담긴 문자열을 Json4s 라이브러리를 사용하여 파싱한다.
    // 실패할 경우에는 예외가 발생할 수 있다.
    val jValue: JValue = JsonMethods.parse(jsonString)

    // 유효성 검사 시작
    checkValueType(valueType, jValue, Seq[String]())
  } catch {
    case error: Exception =>
      Seq(error)
  }

}

