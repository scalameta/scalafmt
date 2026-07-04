package org.scalafmt.bench.scala3

import scala.annotation.tailrec
import scala.collection.mutable

// A synthetic but representative Scala 3 file, written in significant-indentation
// (optional-braces) style so it exercises the Scala-3-specific formatting paths:
// enums, given/using, extensions, opaque types, `end` markers, new control
// syntax, trait parameters, context functions, union/intersection types.

opaque type Meters = Double
object Meters:
  def apply(d: Double): Meters = d
  extension (m: Meters)
    def toDouble: Double = m
    def +(other: Meters): Meters = m + other
    def <(other: Meters): Boolean = m < other
end Meters

enum Shape derives CanEqual:
  case Circle(radius: Meters)
  case Rectangle(width: Meters, height: Meters)
  case Composite(parts: List[Shape])

  def area: Double = this match
    case Circle(r) =>
      val rd = r.toDouble
      math.Pi * rd * rd
    case Rectangle(w, h) => w.toDouble * h.toDouble
    case Composite(parts) =>
      parts.foldLeft(0.0): (acc, shape) =>
        acc + shape.area
end Shape

trait Show[A]:
  extension (a: A) def show: String

object Show:
  given Show[Int] with
    extension (a: Int) def show: String = s"Int($a)"

  given Show[Meters] with
    extension (a: Meters) def show: String = s"${a.toDouble}m"

  given [A](using sa: Show[A]): Show[List[A]] with
    extension (a: List[A])
      def show: String =
        a.map(_.show).mkString("[", ", ", "]")

  def render[A](a: A)(using s: Show[A]): String = a.show
end Show

type Id[A] = A
type StringOrInt = String | Int
type Combined = Show[Int] & Show[Meters]

class Registry[A](initial: Seq[A]):
  private val items = mutable.ArrayBuffer.from(initial)

  def add(a: A): this.type =
    items += a
    this

  def find(p: A => Boolean): Option[A] =
    @tailrec
    def loop(i: Int): Option[A] =
      if i >= items.length then None
      else if p(items(i)) then Some(items(i))
      else loop(i + 1)
    loop(0)

  def foreachIndexed(f: (A, Int) => Unit): Unit =
    for
      i <- items.indices
      a = items(i)
    do f(a, i)

  def total(using num: Numeric[A]): A =
    items.foldLeft(num.zero)(num.plus)
end Registry

given intOrdering: Ordering[Meters] with
  def compare(x: Meters, y: Meters): Int =
    if x < y then -1 else if y < x then 1 else 0

def describe(shape: Shape): String =
  shape match
    case Shape.Circle(r) if r.toDouble > 10 =>
      "a large circle"
    case Shape.Circle(_) => "a circle"
    case Shape.Rectangle(w, h) =>
      if w.toDouble == h.toDouble then "a square"
      else "a rectangle"
    case Shape.Composite(parts) =>
      val n = parts.length
      s"a composite of $n shapes"

object Runner:
  def process(shapes: List[Shape]): Unit =
    val registry = Registry(shapes)
    registry.foreachIndexed: (shape, idx) =>
      val label = describe(shape)
      println(s"$idx: $label with area ${shape.area}")

    val big = registry.find(_.area > 100.0)
    big match
      case Some(shape) => println(s"biggest-ish: ${describe(shape)}")
      case None        => println("nothing large")

  @main def main(): Unit =
    val shapes = List(
      Shape.Circle(Meters(5)),
      Shape.Rectangle(Meters(3), Meters(3)),
      Shape.Composite(List(Shape.Circle(Meters(1)), Shape.Circle(Meters(2)))),
    )
    process(shapes)
    println(Show.render(List(1, 2, 3)))
end Runner
