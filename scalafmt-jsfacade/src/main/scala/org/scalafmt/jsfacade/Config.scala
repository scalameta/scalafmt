package org.scalafmt
package jsfacade

import scala.util.Try

import config._
import rewrite._

object Config {
  import eu.unicredit.shocon

  private[this] def get[A: shocon.Extractor](
      config: shocon.Config.Value)(path: String, default: A): A =
    config.get(path).flatMap(_.as[A]).getOrElse(default)

  def fromHoconString(s: String): Either[String, ScalafmtConfig] = {
    Try(shocon.Config(s)).toEither.left.map(_.toString).map { config =>
      val default = ScalafmtConfig()

      // format: off
      ScalafmtConfig(
        version = get(config)("version", default.version),
        maxColumn = get(config)("maxColumn", default.maxColumn),
        docstrings = config.get("docstrings").flatMap(_.as[String]) match {
          case Some("JavaDoc") => Docstrings.JavaDoc
          case Some("ScalaDoc") => Docstrings.ScalaDoc
          case Some("preserve") => Docstrings.preserve
          case _ => default.docstrings
        },
        optIn = OptIn(
          configStyleArguments = get(config)("optIn.configStyleArguments", default.optIn.configStyleArguments),
          breakChainOnFirstMethodDot = get(config)("optIn.breakChainOnFirstMethodDot", default.optIn.breakChainOnFirstMethodDot),
          selfAnnotationNewline = get(config)("optIn.selfAnnotationNewline", default.optIn.selfAnnotationNewline),
          annotationNewlines = get(config)("optIn.annotationNewlines", default.optIn.annotationNewlines)
        ),
        binPack = BinPack(
          unsafeCallSite = get(config)("binPack.unsafeCallSite", default.binPack.unsafeCallSite),
          unsafeDefnSite = get(config)("binPack.unsafeDefnSite", default.binPack.unsafeDefnSite),
          parentConstructors = get(config)("binPack.parentConstructors", default.binPack.parentConstructors),
          literalArgumentLists = get(config)("binPack.literalArgumentLists", default.binPack.literalArgumentLists),
          literalsMinArgCount = get(config)("binPack.literalsMinArgCount", default.binPack.literalsMinArgCount),
          literalsInclude = get(config)("binPack.literalsInclude", default.binPack.literalsInclude),
          literalsExclude = get(config)("binPack.literalsExclude", default.binPack.literalsExclude)
        ),
        continuationIndent = ContinuationIndent(
          callSite = get(config)("continuationIndent.callSite", default.continuationIndent.callSite),
          defnSite = get(config)("continuationIndent.defnSite", default.continuationIndent.defnSite),
          extendSite = get(config)("continuationIndent.extendSite", default.continuationIndent.extendSite)
        ),
        align = config.get("align").flatMap(_.as[String]) match {
          case Some("none") => Align.none
          case Some("some") => Align.some
          case Some("default") => Align.default
          case Some("most") => Align.most
          case Some("more") => Align.more
          case _ => Align(
            openParenCallSite = get(config)("align.openParenCallSite", default.align.openParenCallSite),
            openParenDefnSite = get(config)("align.openParenDefnSite", default.align.openParenDefnSite),
            tokens = config.get("align.tokens") match {
              case Some(shocon.Config.Array(values)) =>
                (values.map {
                  case shocon.Config.StringLiteral("caseArrow") => AlignToken.caseArrow
                  case shocon.Config.StringLiteral(s @ "applyInfix") => AlignToken(s, ".*")
                  case v => AlignToken(
                    code = v("code").as[String].get,
                    owner = v("owner").as[String].get
                  )
                }: Seq[AlignToken]).toSet
              case Some(shocon.Config.StringLiteral("default")) => AlignToken.default
              case _ => default.align.tokens
            },
            arrowEnumeratorGenerator = get(config)("align.arrowEnumeratorGenerator", default.align.arrowEnumeratorGenerator),
            ifWhileOpenParen = get(config)("align.ifWhileOpenParen", default.align.ifWhileOpenParen),
            tokenCategory = get(config)("align.tokenCategory", default.align.tokenCategory),
            treeCategory = get(config)("align.treeCategory", default.align.treeCategory)
          )
        },
        spaces = Spaces(
          beforeContextBoundColon = config.get("spaces.beforeContextBoundColon").flatMap(_.as[String]) match {
            case Some("Always") => SpaceBeforeContextBound.Always
            case Some("Never") => SpaceBeforeContextBound.Never
            case Some("IfMultipleBounds") => SpaceBeforeContextBound.IfMultipleBounds
            case _ => default.spaces.beforeContextBoundColon
          },
          afterTripleEquals = get(config)("spaces.afterTripleEquals ", default.spaces.afterTripleEquals),
          inImportCurlyBraces = get(config)("spaces.inImportCurlyBraces ", default.spaces.inImportCurlyBraces),
          inParentheses = get(config)("spaces.inParentheses ", default.spaces.inParentheses),
          neverAroundInfixTypes = get(config)("spaces.neverAroundInfixTypes ", default.spaces.neverAroundInfixTypes),
          afterKeywordBeforeParen = get(config)("spaces.afterKeywordBeforeParen ", default.spaces.afterKeywordBeforeParen)
        ),
        lineEndings = config.get("lineEndings").flatMap(_.as[String]) match {
          case Some("unix") => LineEndings.unix
          case Some("windows") => LineEndings.windows
          case Some("preserve") => LineEndings.preserve
          case _ => default.lineEndings
        }, 
        rewriteTokens = get(config)("rewriteTokens", default.rewriteTokens),
        rewrite = RewriteSettings(
          rules = config.get("rewrite.rules") match {
            case Some(shocon.Config.Array(rewrites)) =>
              rewrites.collect {
                case shocon.Config.StringLiteral("RedundantBraces") => RedundantBraces
                case shocon.Config.StringLiteral("RedundantParens") => RedundantParens
                case shocon.Config.StringLiteral("SortImports") => SortImports
                case shocon.Config.StringLiteral("AsciiSortImports") => AsciiSortImports
                case shocon.Config.StringLiteral("PreferCurlyFors") => PreferCurlyFors
                case shocon.Config.StringLiteral("ExpandImportSelectors") => ExpandImportSelectors
                case shocon.Config.StringLiteral("AvoidInfix") => AvoidInfix
              }
            case _ => Nil
          },
          redundantBraces = config.get("rewrite.redundantBraces") match {
            case Some(c: shocon.Config.Object) =>
              val defaultBraces = RedundantBracesSettings()
              RedundantBracesSettings(
                includeUnitMethods = get(c)("includeUnitMethods", defaultBraces.includeUnitMethods),
                maxLines = get(c)("maxLines", defaultBraces.maxLines),
                stringInterpolation = get(c)("stringInterpolation", defaultBraces.stringInterpolation)
              )
            case _ => default.rewrite.redundantBraces
          },
          neverInfix = config.get("rewrite.neverInfix").flatMap(_.as[String]) match {
            case Some("neverInfix") => Pattern.neverInfix
            case _ => Pattern(
              includeFilters = get(config)("rewrite.neverInfix.includeFilters", default.rewrite.neverInfix.includeFilters),
              excludeFilters = get(config)("rewrite.neverInfix.excludeFilters", default.rewrite.neverInfix.excludeFilters)
            )
          }
        ),
        indentOperator = config.get("indentOperator").flatMap(_.as[String]) match {
          case Some("default") => IndentOperator.default
          case Some("akka") => IndentOperator.akka
          case _ => IndentOperator(
            include = get(config)("indentOperator.include", default.indentOperator.include),
            exclude = get(config)("indentOperator.exclude", default.indentOperator.exclude)
          )
        },
        newlines = Newlines(
          neverInResultType = get(config)("newlines.neverInResultType", default.newlines.neverInResultType),
          neverBeforeJsNative = get(config)("newlines.neverBeforeJsNative", default.newlines.neverBeforeJsNative),
          sometimesBeforeColonInMethodReturnType = get(config)("newlines.sometimesBeforeColonInMethodReturnType", default.newlines.sometimesBeforeColonInMethodReturnType),
          penalizeSingleSelectMultiArgList = get(config)("newlines.penalizeSingleSelectMultiArgList", default.newlines.penalizeSingleSelectMultiArgList),
          alwaysBeforeCurlyBraceLambdaParams = get(config)("newlines.alwaysBeforeCurlyBraceLambdaParams", default.newlines.alwaysBeforeCurlyBraceLambdaParams),
          alwaysBeforeTopLevelStatements = get(config)("newlines.alwaysBeforeTopLevelStatements", default.newlines.alwaysBeforeTopLevelStatements),
          afterImplicitKWInVerticalMultiline = get(config)("newlines.afterImplicitKWInVerticalMultiline", default.newlines.afterImplicitKWInVerticalMultiline),
          beforeImplicitKWInVerticalMultiline = get(config)("newlines.beforeImplicitKWInVerticalMultiline", default.newlines.beforeImplicitKWInVerticalMultiline),
          alwaysBeforeElseAfterCurlyIf = get(config)("newlines.alwaysBeforeElseAfterCurlyIf", default.newlines.alwaysBeforeElseAfterCurlyIf),
          afterCurlyLambda = config.get("newlines.afterCurlyLambda").flatMap(_.as[String]) match {
            case Some("preserve") => NewlineCurlyLambda.preserve
            case Some("always") => NewlineCurlyLambda.always
            case Some("never") => NewlineCurlyLambda.never
            case _ => default.newlines.afterCurlyLambda
          }
        ),
        runner = config.get("runner").flatMap(_.as[String]) match {
          case Some("statement") => ScalafmtRunner.statement
          case Some("sbt") => ScalafmtRunner.sbt
          case _ => ScalafmtRunner.default
        },
        indentYieldKeyword = get(config)("indentYieldKeyword", default.indentYieldKeyword),
        importSelectors = config.get("importSelectors").flatMap(_.as[String]) match {
          case Some("noBinPack") => ImportSelectors.noBinPack
          case Some("binPack") => ImportSelectors.binPack
          case Some("singleLine") => ImportSelectors.singleLine
          case _ => ImportSelectors.noBinPack
        },
        unindentTopLevelOperators = get(config)("unindentTopLevelOperators", default.unindentTopLevelOperators),
        includeCurlyBraceInSelectChains = get(config)("includeCurlyBraceInSelectChains", default.includeCurlyBraceInSelectChains),
        assumeStandardLibraryStripMargin = get(config)("assumeStandardLibraryStripMargin", default.assumeStandardLibraryStripMargin),
        danglingParentheses = get(config)("danglingParentheses", default.danglingParentheses),
        poorMansTrailingCommasInConfigStyle = get(config)("poorMansTrailingCommasInConfigStyle", default.poorMansTrailingCommasInConfigStyle),
        verticalMultilineAtDefinitionSite = get(config)("verticalMultilineAtDefinitionSite", default.verticalMultilineAtDefinitionSite),
        onTestFailure = get(config)("onTestFailure", default.onTestFailure),
        encoding = config.get("codec").flatMap(_.as[String]) match {
          case Some(codec) => scala.io.Codec(codec)
          case None => default.encoding
        },
        project = ProjectFiles(
          git = get(config)("project.git", default.project.git),
          files = get(config)("project.files", default.project.files),
          includeFilters = get(config)("project.includeFilters", default.project.includeFilters),
          excludeFilters = get(config)("project.excludeFilters", default.project.excludeFilters)
        )
      )
    }
  }
}
