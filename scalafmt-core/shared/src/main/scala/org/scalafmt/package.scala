package org

package object scalafmt {

  private[scalafmt] implicit class ImplicitSeq[T >: Null <: AnyRef](
      private val obj: Seq[T],
  ) extends AnyVal {
    def headOrNull: T = if (obj.isEmpty) null else obj.head
    def lastOrNull: T = if (obj.isEmpty) null else obj.last
  }

  private[scalafmt] implicit class ImplicitIterator[T >: Null <: AnyRef](
      private val obj: Iterator[T],
  ) extends AnyVal {
    def findOrNull(pred: T => Boolean): T = {
      while (obj.hasNext) {
        val a = obj.next()
        if (pred(a)) return a
      }
      null
    }
  }

  private[scalafmt] implicit class ImplicitIterable[T >: Null <: AnyRef](
      private val obj: Iterable[T],
  ) extends AnyVal {
    def findOrNull(pred: T => Boolean): T = obj.iterator.findOrNull(pred)
  }

  // receiver bound `<: AnyRef` (not just `>: Null`) so a value-type receiver
  // (e.g. a Boolean) can't box to `Any` and silently misbehave -- these are
  // reference-only. (`B` stays `>: Null` so side-effecting `?&&`/`&&&` whose
  // function returns Unit still work.)
  private[scalafmt] implicit class ImplicitNullable[A >: Null <: AnyRef](
      private val obj: A,
  ) extends AnyVal {
    def ??(or: => A): A = if (obj != null) obj else or
    def &&&[B >: Null](or: => B): B = if (obj == null) null else or
    def ===(or: => A): Boolean = (obj ne null) && (obj eq or)
    def =!=(or: => A): Boolean = !(obj === or)
    def nnIf(pred: A => Boolean): A =
      if (obj != null && pred(obj)) obj else null
    def orHas(pred: A => Boolean): Boolean = obj == null || pred(obj)
    def nnHas(pred: A => Boolean): Boolean = obj != null && pred(obj)
    def nnMap[B >: Null](f: A => B): B = if (obj == null) null else f(obj)
    def nnFor(f: A => Unit): Unit = if (obj != null) f(obj)
    def nnFold[B](or: => B)(f: A => B): B = if (obj == null) or else f(obj)
    def nnMapOr[B](f: A => B)(or: => B): B = if (obj == null) or else f(obj)
  }

  sealed trait MaybeBool {
    def asBoolean: Boolean
    def getBoolean(or: Boolean): Boolean
  }
  object MaybeBool {
    def apply(b: Boolean): MaybeBool = if (b) True else False
    case object True extends MaybeBool {
      def asBoolean: Boolean = true
      def getBoolean(or: Boolean): Boolean = true
    }
    case object False extends MaybeBool {
      def asBoolean: Boolean = false
      def getBoolean(or: Boolean): Boolean = false
    }
    case object Maybe extends MaybeBool {
      def asBoolean: Boolean = ???
      def getBoolean(or: Boolean): Boolean = or
    }
  }

}
