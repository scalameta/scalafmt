rewrite.rules = [RedundantParens]
<<< basic
object a {
  def c(b: List[Int]): List[Int] =
    for {
      a <- b
      if ((a))
    } yield a
}
>>>
object a {
  def c(b: List[Int]): List[Int] =
    for {
      a <- b
      if a
    } yield a
}
<<< imbalanced sides
object a {
  def c(b: List[Int]): List[Int] =
    for {
      a <- b
      if ((b ++ b).length >= 2)
    } yield a
}
>>>
object a {
  def c(b: List[Int]): List[Int] =
    for {
      a <- b
      if (b ++ b).length >= 2
    } yield a
}
<<< compound statement
object a {
  def c(b: List[Int]): List[Int] =
    for {
      a <- b
      if (a) || (a)
    } yield a
}
>>>
object a {
  def c(b: List[Int]): List[Int] =
    for {
      a <- b
      if (a) || (a)
    } yield a
}
<<< recursive compound statement
object a {
  def c(b: List[Int]): List[Int] =
    for {
      a <- b
      if ((a)) || ((a))
    } yield a
}
>>>
object a {
  def c(b: List[Int]): List[Int] =
    for {
      a <- b
      if ((a)) || ((a))
    } yield a
}
