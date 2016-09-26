package metaconfig

import scala.collection.immutable.Iterable
import scala.collection.immutable.Seq
import scala.collection.immutable.Set
import scala.reflect.ClassTag

trait Reader[+T] { self =>
  def read(any: Any): Result[T]

  def map[TT](f: T => TT): Reader[TT] = self.flatMap(x => Right(f(x)))

  def flatMap[TT](f: T => Result[TT]): Reader[TT] =
    new Reader[TT] {
      override def read(any: Any) = self.read(any) match {
        case Right(x) => f(x)
        case Left(x) => Left(x)
      }
    }
}

object Reader {

  def fail[T: ClassTag](x: Any): Result[T] = {
    val clz = x.getClass.getSimpleName
    Left(new IllegalArgumentException(s"value '$x' of type $clz."))
  }

  def instance[T](f: PartialFunction[Any, Result[T]])(
      implicit ev: ClassTag[T]) =
    new Reader[T] {
      override def read(any: Any): Result[T] = {
        try {
          if (ev.runtimeClass.getTypeParameters.isEmpty && // careful with erasure
              ev.runtimeClass.isInstance(any)) {
            Right(any.asInstanceOf[T])
          } else {
            f.applyOrElse(any, (x: Any) => fail[T](x))
          }
        } catch {
          case e: ConfigError => Left(e)
        }
      }
    }
  implicit val intR = instance[Int] { case x: Int => Right(x) }
  implicit val stringR = instance[String] { case x: String => Right(x) }
  implicit val boolR = instance[scala.Boolean] {
    case x: Boolean => Right(x)
  }
  implicit def seqR[T](implicit ev: Reader[T]): Reader[Seq[T]] =
    instance[Seq[T]] {
      case lst: Seq[_] =>
        val res = lst.map(ev.read)
        val lefts = res.collect { case Left(e) => e }
        if (lefts.nonEmpty) Left(ConfigErrors(lefts))
        else Right(res.collect { case Right(e) => e })
    }

  implicit def setR[T](implicit ev: Reader[T]): Reader[Set[T]] =
    instance[Set[T]] {
      case lst: Set[_] => Right(lst.asInstanceOf[Set[T]])
      case lst: Seq[_] =>
        val res = lst.map(ev.read)
        val lefts = res.collect { case Left(e) => e }
        if (lefts.nonEmpty) Left(ConfigErrors(lefts))
        else Right(res.collect({ case Right(e) => e }).to[Set])
    }

  // TODO(olafur) generic can build from reader
  implicit def mapR[K, V](implicit evK: Reader[K],
                          evV: Reader[V]): Reader[Map[K, V]] =
    instance[Map[K, V]] {
      case map: Map[_, _] =>
        val res = map.map {
          case (k, v) =>
            for {
              kk <- evK.read(k).right
              vv <- evV.read(v).right
            } yield (kk, vv)
        }
        val sRes = Seq(res.toSeq: _*)
        val lefts: Seq[Throwable] = sRes.collect {
          case Left(e) => e
        }
        if (lefts.nonEmpty) Left(ConfigErrors(lefts.toSeq))
        else {
          val resultMap = sRes.collect { case Right(e) => e }
          Right(resultMap.toMap)
        }
    }
}
