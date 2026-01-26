package org.scalafmt.internal

import scala.meta.tokens.{Token => T}

import scala.collection.{immutable, mutable}
import scala.reflect.ClassTag

/* scalafmt: {
     binPack.defnSite = always
     binPack.callSite = always
     newlines.configStyle.fallBack.prefer = false
   }
 */
object SplitsBuilder {

  type LookupMap = immutable.Map[Class[_], Splits]

  def build(f: SplitsBuilder => Unit): LookupMap = {
    val builder = new SplitsBuilder
    f(builder)
    builder.result()
  }

  val lookupAfter: LookupMap = build { builder =>
    builder.add[T.BOF](SplitsAfterBOF)
    builder.add[T.EOF](SplitsAfterEOF)
    builder.add[T.Shebang](SplitsNewline2X)
    builder.add[T.Interpolation.Start](SplitsAfterInterpolationStart)
    builder.add[T.Xml.Part, T.MacroQuote, T.Interpolation.Id,
      T.Interpolation.Part, T.Interpolation.SpliceStart,
    ](SplitsNoSplit)
    builder.add[T.Dot](SplitsAfterDot)
    builder.add[T.LeftBrace](SplitsAfterLeftBrace)
    builder.add[T.LeftParen](SplitsAfterLeftParen)
    builder.add[T.LeftBracket](SplitsAfterLeftBracket)
    builder.add(classOf[T.Equals], classOf[T.Colon], classOf[T.KwWith],
      classOf[T.RightParen], classOf[T.KwReturn], classOf[T.RightArrow],
      classOf[T.ContextArrow], classOf[T.LeftArrow], classOf[T.KwMatch],
      classOf[T.KwThen], classOf[T.KwElse], classOf[T.KwThrow], classOf[T.KwTry],
      classOf[T.KwCatch], classOf[T.KwFinally], classOf[T.KwFor], classOf[T.KwDo],
      classOf[T.KwWhile], classOf[T.KwYield], classOf[T.KwIf],
    )(SplitsAfterOptionalBracesKeyword)
    builder.add[T.Equals](SplitsAfterEquals)
    builder.add[T.LeftArrow](SplitsAfterLeftArrow)
    builder.add[T.RightArrow, T.ContextArrow](SplitsAfterFunctionArrow)
    builder.add[T.RightArrow](SplitsAfterRightArrow)
    builder.add[T.KwMatch](SplitsAfterMatch)
    builder.add[T.KwGiven](SplitsAfterGiven)
    builder.add[T.Semicolon](SplitsAfterSemicolon)
    builder.add[T.KwObject, T.KwClass, T.KwTrait, T.KwEnum](
      SplitsAfterTemplateKeyword,
    )
    builder.add[T.Colon](SplitsAfterColon)
    builder.add[T.Comma](SplitsAfterComma)
    builder.add[T.RightParen](SplitsAfterRightParen)
    builder.add[T.KwIf](SplitsAfterIf)
    builder.add[T.At](SplitsAfterAt)
    builder.add[T.Ident](SplitsAfterIdent)
    builder.add[T.KwWhile, T.KwFor](SplitsAfterForWhile)
    builder.add[T.KwDo](SplitsAfterDo)
    builder.add[T.KwElse](SplitsAfterElse)
    builder.add[T.KwCase](SplitsAfterCase)
    builder.add[T.KwTry](SplitsAfterTry)
    builder.add[T.KwCatch](SplitsAfterCatch)
    builder.add[T.KwFinally](SplitsAfterFinally)
    builder.add[T.KwYield](SplitsAfterYield)
    builder.add[T.KwDef, T.KwPackage](SplitsSpace)
    builder.add[T.Xml.Start](SplitsAfterXmlStart)
    builder.add[T.Xml.SpliceStart](SplitsAfterXmlSpliceStart)
    builder.add[T.KwImplicit](SplitsAfterImplicit)
  }

  val lookupBefore: LookupMap = build { builder =>
    builder.add[T.BOF](SplitsNewline)
    builder.add[T.EOF](SplitsNewline)
    builder.add[T.Xml.Part, T.Interpolation.Part, T.Interpolation.End,
      T.Interpolation.SpliceEnd,
    ](SplitsNoSplit)
    builder.add[T.LeftBrace](SplitsBeforeLeftBrace)
    builder.add[T.RightBrace](SplitsBeforeRightBrace)
    builder.add[T.RightParen](SplitsBeforeRightParen)
    builder.add[T.RightBracket](SplitsBeforeRightBracket)
    builder.add[T.RightArrow](SplitsBeforeRightArrow)
    builder.add[T.Semicolon](SplitsBeforeSemicolon)
    builder.add[T.LeftParen](SplitsBeforeLeftParenOrBracket)
    builder.add[T.LeftBracket](SplitsBeforeLeftParenOrBracket,
      SplitsBeforeLeftBracket,
    )
    builder.add[T.Colon](SplitsBeforeColon)
    builder.add[T.Subtype](SplitsBeforeSubtype)
    builder.add[T.Supertype](SplitsBeforeSupertype)
    builder.add[T.Viewbound](SplitsBeforeViewbound)
    builder.add[T.Dot](SplitsBeforeDot)
    builder.add[T.KwExtends](SplitsBeforeExtends)
    builder.add[T.KwWith](SplitsBeforeWith)
    builder.add[T.Ident](SplitsBeforeIdent)
    builder.add[T.KwIf](SplitsBeforeIf)
    builder.add[T.KwWhile](SplitsBeforeWhile)
    builder.add[T.KwElse, T.KwYield](SplitsBeforeElseYield)
    builder.add[T.KwThen](SplitsBeforeThen)
    builder.add[T.KwDo](SplitsBeforeDo)
    builder.add[T.KwMatch](SplitsBeforeMatch)
    builder.add[T.At](SplitsBeforeAt)
    builder.add[T.Comma](SplitsBeforeComma)
    builder.add[T.Hash](SplitsBeforeHash)
    builder.add[T.KwCatch, T.KwFinally](SplitsBeforeCatchFinally)
  }

  val lookupBeforeLowPriority: LookupMap = build { builder =>
    builder.add[T.Xml.Start, T.Interpolation.Id](SplitsSpace)
    builder.add[T.Comment](SplitsBeforeCommentLowPriority)
  }
  val lookupAfterLowPriority: LookupMap = build { builder =>
    builder.add[T.RightBrace](SplitsAfterRightBraceLowPriority)
    builder.add[T.KwReturn](SplitsAfterReturnLowPriority)
    builder.add[T.Comment](SplitsAfterCommentLowPriority)
    builder.add[T.Dot](SplitsAfterDotLowPriority)
  }
}

private[internal] class SplitsBuilder {
  private val map = mutable.Map.empty[Class[_], mutable.ListBuffer[Splits]]

  def add(cls: Class[_]*)(gen: Splits*): Unit = cls
    .foreach(cls => map.getOrElseUpdate(cls, mutable.ListBuffer.empty) ++= gen)
  def add[T](gen: Splits*)(implicit ct: ClassTag[T]): Unit =
    add(ct.runtimeClass)(gen: _*)
  def add[T1: ClassTag, T2: ClassTag](gen: Splits*): Unit = {
    add[T1](gen: _*)
    add[T2](gen: _*)
  }
  def add[T1: ClassTag, T2: ClassTag, T3: ClassTag](gen: Splits*): Unit = {
    add[T1, T2](gen: _*)
    add[T3](gen: _*)
  }
  def add[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag](
      gen: Splits*,
  ): Unit = {
    add[T1, T2, T3](gen: _*)
    add[T4](gen: _*)
  }
  def add[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag](
      gen: Splits*,
  ): Unit = {
    add[T1, T2, T3, T4](gen: _*)
    add[T5](gen: _*)
  }
  def add[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag,
      T6: ClassTag,
  ](gen: Splits*): Unit = {
    add[T1, T2, T3, T4, T5](gen: _*)
    add[T6](gen: _*)
  }

  def result(): SplitsBuilder.LookupMap = {
    val builder = immutable.Map.newBuilder[Class[_], Splits]
    map.foreach { case (k, v) => builder += k -> Splits(v.toList) }
    builder.result()
  }

}
