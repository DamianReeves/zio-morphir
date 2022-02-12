package zio.morphir.syntax

import zio.{Chunk, ZEnvironment}
import zio.morphir.ir.{Literal => Lit, Name, ValueModule}
import ValueModule.{RawValue, Value}
import java.math.BigInteger
import zio.morphir.ir.FQName

trait ValueSyntax {
  import Value.{List => _, *}

  final def apply(function: RawValue, args: RawValue*): Apply[Any] =
    Apply(function, Chunk.fromIterable(args), ZEnvironment.empty)

  def asPattern(pattern: Pattern[Any], variableName: String): Pattern.AsPattern[Any] =
    Pattern.asPattern(pattern, Name.fromString(variableName))

  def asPattern(pattern: Pattern[Any], variableName: Name): Pattern.AsPattern[Any] =
    Pattern.asPattern(pattern, variableName)

  final def boolean(value: Boolean): Literal[Boolean, Any] = Literal(Lit.boolean(value), ZEnvironment.empty)
  final def boolean[Annotations](
      value: Boolean,
      annotations: ZEnvironment[Annotations]
  ): Literal[Boolean, Annotations] =
    Literal(Lit.boolean(value), annotations)

  final def constructorPattern(
      constructorName: FQName,
      argumentPatterns: Pattern[Any]*
  ): Pattern.ConstructorPattern[Any] =
    Pattern.ConstructorPattern(constructorName, Chunk.fromIterable(argumentPatterns), ZEnvironment.empty)

  final def field(name: Name, record: Record[Any]): Field[Any] = Field(record, name, ZEnvironment.empty)
  final def field(name: String, record: Record[Any]): Field[Any] =
    Field(record, Name.fromString(name), ZEnvironment.empty)

  final def fieldFunction(name: Name): FieldFunction[Any] = FieldFunction(name, ZEnvironment.empty)
  final def fieldFunction(name: String): FieldFunction[Any] =
    FieldFunction(Name.fromString(name), ZEnvironment.empty)

  final def int(value: Int): Literal[BigInteger, Any] = Literal(Lit.int(value), ZEnvironment.empty)

  final def lambda(pattern: Value.Pattern[Any], body: Value[Any]): Lambda[Any] =
    Lambda(pattern, body, ZEnvironment.empty)

  final def list(values: Value[Any]*): Value.List[Any] = Value.List(Chunk.fromIterable(values), ZEnvironment.empty)
  final def literal[V](value: Lit[V]): Literal[V, Any] = Literal(value, ZEnvironment.empty)
  final def literal[V, Annotations](value: Lit[V], annotations: ZEnvironment[Annotations]): Literal[V, Annotations] =
    Literal(value, annotations)

  final def long(value: Long): Literal[BigInteger, Any] = Literal(Lit.long(value), ZEnvironment.empty)

  def patternMatch(scrutinee: Value[Any], cases: (Value[Any], Value[Any])*): PatternMatch[Any] =
    PatternMatch(scrutinee, Chunk.fromIterable(cases), ZEnvironment.empty)

  def record(fields: (Name, Value[Any])*): Record[Any] = Record(Chunk.fromIterable(fields), ZEnvironment.empty)
  def record(field: (String, Value[Any]), otherFields: (String, Value[Any])*): Record[Any] = {
    val fields = (Chunk.single(field) ++ Chunk.fromIterable(otherFields)).map { case (name, value) =>
      Name.fromString(name) -> value
    }
    Record(fields, ZEnvironment.empty)
  }

  final def string(value: String): Literal[String, Any] = Literal(Lit.string(value), ZEnvironment.empty)
  final def string[Annotations](value: String, annotations: ZEnvironment[Annotations]): Value[Annotations] =
    Literal(Lit.string(value), annotations)

  final val unit: Unit[Any]                                                              = Unit(ZEnvironment.empty)
  final def unit[Annotations](annotations: ZEnvironment[Annotations]): Unit[Annotations] = Unit(annotations)

  final def variable(name: Name): Variable[Any]   = Variable(name, ZEnvironment.empty)
  final def variable(name: String): Variable[Any] = Variable(Name.fromString(name), ZEnvironment.empty)

  final def wholeNumber(value: java.math.BigInteger): Value.Literal[java.math.BigInteger, Any] = {
    println("In Value.wholeNumber")
    Value.Literal(Lit.wholeNumber(value), ZEnvironment.empty)
  }

  val wildcardPattern: Pattern.WildcardPattern[Any] = Pattern.wildcardPattern
  @inline final def wildcardPattern[Annotations](
      annotations: ZEnvironment[Annotations]
  ): Pattern.WildcardPattern[Annotations] =
    Pattern.wildcardPattern(annotations)

}
