---
id: introduction
title: Introduction
---

> Any style guide written in English is either so brief that it’s ambiguous, or
> so long that no one reads it.
> <cite>- Bob Nystrom, [Hardest Program I've Ever Written][bob], Dart, Google.</cite>

`scalafmt` turns the mess on left into the (hopefully) readable, idiomatic and
consistently formatted Scala code on the right

```tut:passthrough
website.formatExample(
  s"""|object FormatMe { List(number) match
      |{ case head :: Nil
      |if head % 2 == 0 => "number is even"
      |  case head :: Nil =>
      |  "number is not even"
      |  case Nil =>
      |  "List is empty" }
      |  function(arg1,
      |  arg2(arg3(arg4,
      |  arg5, "arg6")
      |  , arg7 + arg8),
      |  arg9.select(1, 2,
      |  3, 4, 5, 6)) }""".stripMargin
)
```

The goal of scalafmt is to produce good enough formatted code so that you can
focus on programming instead of manipulating syntax trivia. Scalafmt can be used
in several environments such as the command line, text editors and build tools.

It is not a goal to format every single Scala source file under the sun. In
particular, scalafmt cannot format deeply nested computer generated code.

Scalafmt is maintained by [@olafurpg](https://twitter.com/olafurpg) in his free
time. Bug reports, feature requests, questions and PRs are welcome. Complaints
and unfriendly attitude is not welcome.

Curious to learn more about scalafmt? Check out this talk:

<iframe src="https://player.vimeo.com/video/165929294" width="100%" style="height: 28em;" frameborder="0" webkitallowfullscreen mozallowfullscreen allowfullscreen></iframe>

[bob]: http://journal.stuffwithstuff.com/2015/09/08/the-hardest-program-ive-ever-written/
