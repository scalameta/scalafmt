package org.scalafmt
import scala.util.{Either, Failure, Right, Success, Try}

package object dynamic {

  implicit class EitherOps[+A, +B](val target: Either[A, B]) extends AnyVal {
    def flatMap[A1 >: A, B1](f: B => Either[A1, B1]): Either[A1, B1] =
      target match {
        case Right(b) => f(b)
        case _ => target.asInstanceOf[Either[A1, B1]]
      }

    def map[B1](f: B => B1): Either[A, B1] = target match {
      case Right(b) => Right(f(b))
      case _ =>
        target.asInstanceOf[Either[A, B1]]
    }
  }

  implicit class TryOps[T](val target: Try[T]) extends AnyVal {
    def toEither: Either[Throwable, T] =
      target match {
        case Success(value) => Right(value)
        case Failure(ex) => Left(ex)
      }
  }

}
