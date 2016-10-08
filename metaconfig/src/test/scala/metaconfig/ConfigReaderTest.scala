package metaconfig

import org.scalatest.FunSuite

class ConfigReaderTest extends FunSuite {
  type Result[T] = Either[Throwable, T]

  @ConfigReader
  case class Inner(nest: Int)

  @ConfigReader
  case class Outer(i: Int, inner: Inner) {
    implicit val innerReader: Reader[Inner] = inner.reader
  }

  @ConfigReader
  case class Bar(i: Int, b: Boolean, s: String)

  @ConfigReader
  case class HasList(i: Seq[Int])

  @ConfigReader
  case class HasMap(i: Map[Int, Int])

  val b = Bar(0, true, "str")
  test("invalid field") {
    assert(
      b.reader.read(Map("is" -> 2, "var" -> 3)) ==
        Left(ConfigError("Invalid fields: is, var")))
  }

  test("read OK") {
    assert(b.reader.read(Map("i" -> 2)) == Right(b.copy(i = 2)))
    assert(b.reader.read(Map("s" -> "str")) == Right(b.copy(s = "str")))
    assert(b.reader.read(Map("b" -> true)) == Right(b.copy(b = true)))
    assert(
      b.reader.read(
        Map(
          "i" -> 3,
          "b" -> true,
          "s" -> "rand"
        )) == Right(b.copy(i = 3, s = "rand", b = true)))
  }
  test("unexpected type") {
    val msg =
      "Error reading field 'i' on class Bar. Expected argument of type int. Obtained value 'str' of type String."
    assert(b.reader.read(Map("i" -> "str")) == Left(ConfigError(msg)))
  }

  test("write OK") {
    assert(
      b.fields == Map(
        "i" -> 0,
        "b" -> true,
        "s" -> "str"
      ))
  }
  test("nested OK") {
    val m: Map[String, Any] = Map(
      "i" -> 4,
      "inner" -> Map(
        "nest" -> 5
      )
    )
    val o = Outer(2, Inner(3)).reader.read(m)
    println(o)
  }

  test("Seq") {
    val lst = HasList(List(1, 2, 3))
    assert(
      lst.reader.read(Map("i" -> Seq(666, 777))) == Right(
        HasList(Seq(666, 777))))
  }

  test("Map") {
    val lst = HasMap(Map(1 -> 2))
    assert(
      lst.reader.read(Map("i" -> Map(666 -> 777))) ==
        Right(HasMap(Map(666 -> 777))))
  }

  test("inner") {
//    val m: Map[String, Any] = Map(
//      "i" -> 4
//      )
//    )
//    val o = Outer(2, Inner(3)).reader.read(m)

  }

}
