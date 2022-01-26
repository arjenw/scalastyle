// Copyright (C) 2011-2012 the original author or authors.
// See the LICENCE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package org.scalastyle.scalariform

import _root_.scalariform.lexer.Tokens.{DOT, PACKAGE, VARID}
import _root_.scalariform.lexer.{MultiLineComment, Token, Whitespace}
import _root_.scalariform.parser.{
  AstNode,
  BlockImportExpr,
  CompilationUnit,
  Expr,
  ExprElement,
  GeneralTokens,
  ImportClause,
  ImportSelectors
}
import org.scalastyle.scalariform.VisitorHelper.visit
import org.scalastyle.{FileSpec, Level, Lines, Message, PositionError, ScalariformChecker, ScalastyleError}

import java.util.regex.Pattern
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.Try
import scala.util.matching.Regex
import scala.xml.{Attribute, Elem, InputSource, Node, Source, XML}

// scalastyle:off multiple.string.literals

abstract class AbstractImportChecker extends ScalariformChecker {
  case class ImportClauseVisit(
    t: ImportClause,
    importExpr: List[ImportClauseVisit],
    otherImportExprs: List[ImportClauseVisit]
  )

  def verify(ast: CompilationUnit): List[ScalastyleError] = {
    init()

    val it = for {
      t <- localvisit(ast.immediateChildren)
      f <- traverse(t)
    } yield PositionError(t.t.firstToken.offset)

    it
  }

  protected def init(): Unit = {}

  private[this] def traverse(t: ImportClauseVisit): List[ImportClauseVisit] = {
    val l = t.importExpr.flatMap(traverse) ::: t.otherImportExprs.flatMap(traverse)
    if (matches(t)) t :: l else l
  }

  private[this] def imports(tokens: List[Token]): String =
    tokens.foldLeft("")((a, b) => a + b.text)

  private[this] def imports(t: BlockImportExpr): List[String] = {
    val is = t.importSelectors

    val firsts = is.firstImportSelector.firstToken.text ::
      is.otherImportSelectors.map(_._2).map(is => is.firstToken.text)
    firsts.map(f => imports(t.prefixExpr.tokens) + f)
  }

  protected final def imports(t: ImportClauseVisit): List[String] = {
    t.t.importExpr match {
      case t: BlockImportExpr => imports(t)
      case _                  => List(imports(t.t.importExpr.tokens))
    }
  }

  def matches(t: ImportClauseVisit): Boolean

  private[this] def localvisit(ast: Any): List[ImportClauseVisit] =
    ast match {
      case t: ImportClause =>
        List(ImportClauseVisit(t, localvisit(t.importExpr), localvisit(t.otherImportExprs)))
      case t: Any => visit(t, localvisit)
    }
}

class IllegalImportsChecker extends AbstractImportChecker {
  val errorKey = "illegal.imports"

  val DefaultIllegalImports = "sun._"
  var illegalImportsList: List[String] = _
  var exemptImportsList: List[String] = _

  // sun._ => sun\.
  // sun.com.foobar => sun\.com\.foobar
  private def toMatchList(s: String) =
    s.trim().split(" *, *").map(s => s.replaceAll("_$", "")).toList

  override protected def init() = {
    illegalImportsList = toMatchList(getString("illegalImports", DefaultIllegalImports))
    exemptImportsList = toMatchList(getString("exemptImports", ""))
  }

  def matches(t: ImportClauseVisit): Boolean = {
    val list = imports(t)
    val revisedList = list diff exemptImportsList
    illegalImportsList.exists(ill => revisedList.exists(_.startsWith(ill)))
  }
}

class UnderscoreImportChecker extends AbstractImportChecker {
  private val DefaultIgnoreRegex = "^$"
  val errorKey = "underscore.import"

  private var ignoreRegex: Regex = _

  override protected def init(): Unit =
    ignoreRegex = getString("ignoreRegex", DefaultIgnoreRegex).r

  def matches(t: ImportClauseVisit): Boolean =
    imports(t)
      .filterNot((importStatement) => ignoreRegex.findFirstIn(importStatement).isDefined)
      .exists(_.endsWith("._"))
}

class ImportGroupingChecker extends ScalariformChecker {
  val errorKey = "import.grouping"

  def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val it = VisitorHelper.getAll[ImportClause](ast.immediateChildren)

    if (it.isEmpty)
      List()
    else {
      val importTokens = it.flatMap(_.tokens)
      val (min, max) = (importTokens.head.offset, importTokens.last.offset)

      val s = ast.tokens.find(t =>
        t.offset >= min && t.offset <= max && !t.isNewline && !(t.text == ";") && !importTokens.contains(t)
      )

      s match {
        case Some(x) =>
          it.dropWhile(ic => ic.firstToken.offset <= x.offset).map(ic => PositionError(ic.firstToken.offset))
        case None => List()
      }
    }
  }
}

/**
 * Style checker that enforces import ordering. The following configuration parameters are available:
 *
 *   - groups: a comma-separated list of group names to consider.
 *   - maxBlankLines: maximum number of blank lines to allow between groups. The default is "1". A value less
 *     than 1 disables the blank line limit.
 *   - group.[groupName]: a regular expression that matches imports that should be in the given group.
 *   - lexicographic: if true, imports are ordered lexicographically (classes, wildcards, then packages;
 *     case-sensitive ordering within); if false, apply the original case-insensitive ordering (with wildcards
 *     coming first, before classes).
 *
 * For example, to check that "java" and "javax" imports are in a separate group at the top of the import
 * list, you'd use this config:
 *
 * <parameter name="groups">java,others</parameter> <parameter name="group.java">javax?\..+</parameter>
 * <parameter name="group.other">.+</parameter>
 *
 * Other non-configurable rules:
 *   - Within each group, import clauses are ordered alphabetically if 'lexicographic' is specified; else puts
 *     wildcards, then classes and packages, with case-insensitive sort.
 *   - In multi-import statements, entries are ordered alphabetically, with method / packages (assumed to be
 *     any string starting with a lower case letter) coming before classes.
 *
 * Currently, this checker only looks at the top-level list of imports.
 */
class ImportOrderChecker extends ScalariformChecker {
  val errorKey: String = "import.ordering"

  private var groups: Seq[(String, Pattern)] = _
  private var maxBlankLines: Int = _
  private var lexicographic: Boolean = _

  private var ast: AstNode = _
  private var lastImport: Option[AstNode] = None

  private var currentGroup = 0
  private var lastImportInGroup: Option[String] = None

  override def setParameters(parameters: Map[String, String]): Unit = {
    // Note that any exceptions thrown here are swallowed by CheckerUtils and ignored...
    require(parameters.contains("groups"))
    groups = parameters("groups").split(",").toSeq.map { name =>
      name -> Pattern.compile(parameters(s"group.$name"))
    }
    maxBlankLines = parameters.getOrElse("maxBlankLines", "1").toInt
    lexicographic = parameters.get("lexicographic").map(_.toBoolean).getOrElse(false)
  }

  override def verify(ast: CompilationUnit): List[ScalastyleError] = {
    this.ast = ast

    val CompilationUnit(statements, _) = ast
    statements.immediateChildren.flatMap { n =>
      val result = n match {
        case ImportClause(_, BlockImportExpr(prefix, selectors), _, _) =>
          val text = exprToText(prefix.contents, if (lexicographic) selectors.tokens else Nil)
          checkImport(text, n.firstToken.offset) ++ checkSelectors(selectors)

        case ImportClause(_, Expr(contents), _, _) =>
          val text = exprToText(contents)
          checkImport(text, n.firstToken.offset)

        case _ =>
          Nil
      }
      lastImport = Some(n)
      result
    }
  }

  private def exprToText(contents: List[ExprElement], extra: List[Token] = Nil): String = {
    val contentsTokens = contents.flatMap {
      case GeneralTokens(toks) => toks
      case n: Any              => throw new IllegalStateException(s"FIXME: unexpected expr child node $n")
    }
    (contentsTokens ++ extra).iterator.map(_.text).mkString("")
  }

  /**
   * Check that the given import belongs to the current group and is ordered correctly within it.
   */
  private def checkImport(str: String, offset: Int): Seq[ScalastyleError] = {
    val errors = new ListBuffer[ScalastyleError]()

    if (!groups(currentGroup)._2.matcher(str).matches()) {
      // If a statement doesn't match the current group, there are two options:
      // - It belongs to a previous group, in which case an error is flagged.
      // - It belongs to a following group, in which case the group index moves forward.
      for (i <- 0 until currentGroup)
        if (groups(i)._2.matcher(str).matches())
          return Seq(newError(offset, "wrongGroup", str, groups(i)._1, groups(currentGroup)._1))

      var nextGroup = currentGroup + 1
      while (nextGroup < groups.size && !groups(nextGroup)._2.matcher(str).matches())
        nextGroup += 1

      if (nextGroup == groups.size)
        throw new IllegalStateException(s"FIXME: import statement does not match any group: $str")

      errors ++= checkGroupSeparation(currentGroup, nextGroup, offset)
      currentGroup = nextGroup
      lastImportInGroup = None
    } else
      // If the statement is in the same group, make sure there is no empty line between it and
      // the previous import.
      errors ++= checkNoSeparator(offset)

    // Ensure import is in alphabetical order.
    if (lastImportInGroup.isDefined && compareImports(lastImportInGroup.get, str) > 0)
      errors += newError(offset, "wrongOrderInGroup", str, lastImportInGroup.get)

    lastImportInGroup = Some(str)
    errors.toSeq
  }

  /**
   * Check that the imports inside a multi-import block are ordered.
   */
  private def checkSelectors(selectors: ImportSelectors): Seq[ScalastyleError] = {
    val ImportSelectors(_, first, others, _) = selectors

    val errors = new ListBuffer[ScalastyleError]()
    val names = Seq(first.contents.head.tokens.head.text) ++
      others.map(_._2.contents.head.tokens.head.text)

    if (names.size > 1) {
      names.sliding(2).foreach { case Seq(left, right) =>
        if (compareNames(left, right, isImport = false) > 0)
          errors += newError(selectors.firstToken.offset, "wrongOrderInSelector", right, left)
      }
    }

    errors.toSeq
  }

  /**
   * When the current import group changes, checks that there is a single empty line between the last import
   * statement in the previous group and the first statement in the new one.
   */
  private def checkGroupSeparation(
    lastGroup: Int,
    nextGroup: Int,
    nextGroupOffset: Int
  ): Option[ScalastyleError] = {
    if (lastGroup != nextGroup && lastImport.isDefined) {
      val start = lastImport.get.lastToken.offset + lastImport.get.lastToken.length
      val separatorLines = countNewLines(start, nextGroupOffset) - 1
      val last = groups(lastGroup)._1
      val current = groups(nextGroup)._1
      if (separatorLines == 0)
        return Some(newError(nextGroupOffset, "missingEmptyLine", last, current))
      else if (maxBlankLines > 0 && separatorLines > maxBlankLines)
        return Some(newError(nextGroupOffset, "tooManyEmptyLines", maxBlankLines, last, current))
    }

    None
  }

  /**
   * Check that there are no empty lines between imports in the same group.
   */
  private def checkNoSeparator(offset: Int): Option[ScalastyleError] = {
    if (lastImportInGroup.isDefined) {
      val start = lastImport.get.lastToken.offset + lastImport.get.lastToken.length
      if (countNewLines(start, offset) != 1)
        return Some(newError(offset, "noEmptyLine"))
    }
    None
  }

  /**
   * Counts the number of new lines between the given offsets, adjusted for comments.
   */
  private def countNewLines(start: Int, end: Int): Int = {
    var count = 0
    ast.tokens
      .filter(t => t.offset >= start && t.offset < end)
      .foreach { t =>
        val commentsToken = t.associatedWhitespaceAndComments
        if (commentsToken != null) { // scalastyle:ignore null
          var ignoreNext = false
          commentsToken.tokens.foreach {
            case c: MultiLineComment =>
              // Do not count a new line after a multi-line comment.
              ignoreNext = true
            case w: Whitespace =>
              if (!ignoreNext)
                // Assumes "\n" only used for new lines.
                count += w.text.count(_ == '\n')
              ignoreNext = true
            case _ =>
            // Nothing to do.
          }
        }
      }
    count
  }

  /**
   * Compares two import statements, comparing each component of the import separately.
   *
   * The import statements can end with a dangling `.`, meaning they're the start of a multi-import block.
   */
  private[scalariform] def compareImports(imp1: String, imp2: String): Int = {
    val imp1Components = imp1.split("[.]")
    val imp2Components = imp2.split("[.]")
    val max = math.min(imp1Components.size, imp2Components.size)
    for (i <- 0 until max) {
      val comp1 = imp1Components(i)
      val comp2 = imp2Components(i)
      val result = compareNames(comp1, comp2, isImport = true)
      if (result != 0)
        return result
    }

    // At this point, there is still a special case: where one import is a multi-import block
    // (and, thus, has no extra components) and another is a wildcard; the wildcard should come
    // first.
    val diff = imp1Components.size - imp2Components.size
    if (diff == -1 && imp1.endsWith(".") && imp2Components.last == "_")
      1
    else if (diff == 1 && imp2.endsWith(".") && imp1Components.last == "_")
      -1
    else
      diff
  }

  /**
   * Compares two strings that represent a single imported artifact; this considers lower-case names as being
   * "lower" than upper case ones.
   *
   * @param name1
   *   First name.
   * @param name2
   *   Second name.
   * @param isImport
   *   If true, orders names according to the import statement rules: "_" should come before other names, and
   *   capital letters should come before lower case ones. Otherwise, do the opposite, which are the ordering
   *   rules for names within a selector.
   */
  private[scalariform] def compareNames(name1: String, name2: String, isImport: Boolean): Int = {
    if (lexicographic && isImport)
      name1.compareTo(name2)
    else if (name1 != "_") {
      if (name2 == "_")
        -1 * compareNames(name2, name1, isImport)
      else {
        val isName1UpperCase = Character.isUpperCase(name1.codePointAt(0))
        val isName2UpperCase = Character.isUpperCase(name2.codePointAt(0))

        if (isName1UpperCase == isName2UpperCase)
          name1.compareToIgnoreCase(name2)
        else {
          // Classes come before subpackages in import statements, after in selectors.
          val order = if (isImport) -1 else 1
          if (isName1UpperCase) order else -order
        }
      }
    } else if (isImport) -1
    else 1
  }

  private def newError(offset: Int, errorKey: String, args: Any*): ScalastyleError =
    PositionError(offset, args.map(_.toString).toList, Some(this.errorKey + "." + errorKey))

}

/**
 * Style checker that controls what can be imported in each package. The following configuration parameters
 * are available:
 *
 *   - file: a reference to the import-control configuration
 *
 * Currently, this checker only looks at the top-level list of imports.
 *
 * Note that the file format is defined in a dtd (see 'import_control_1_4.dtd' in resources) and is the same
 * as the Checkstyle ImportControl configuration. As such the same configuration can be used for Checkstyle
 * and Scalastyle (useful for mixed projects). See
 * https://checkstyle.sourceforge.io/config_imports.html#ImportControl for more details.
 */
class ImportControlChecker extends ScalariformChecker {

  import ImportControlConfig._

  val errorKey: String = "import.control"
  // split in part before ., (optional) middle part with trailing dot, last part after dot
  private val importDecomposeRe = "(.*)\\.(.*\\.)*(.*)".r
  private var config: Try[ImportControlConfig] = _

  override def setParameters(parameters: Map[String, String]): Unit = {
    super.setParameters(parameters)

    config = Try {
      require(parameters.contains("file") || parameters.contains("inline"))

      val source =
        parameters
          .get("file")
          .map { fileName =>
            val file = new java.io.File(fileName)
            require(file.exists(), s"File '${file.getAbsolutePath}' does not exist.")
            Source.fromFile(file)
          }
          .getOrElse(Source.fromString(parameters("inline")))
      apply(source)
    }
  }

  override def verify(ast: CompilationUnit): List[ScalastyleError] =
    throw new UnsupportedOperationException("Wrong method called")

  override def verify[T <: FileSpec](
    file: T,
    level: Level,
    ast: CompilationUnit,
    lines: Lines
  ): List[Message[T]] =
    verify(file.name, ast).map(p => toStyleError(file, p, level, lines))

  def verify(fileName: String, ast: CompilationUnit): List[ScalastyleError] = {
    val name = extractFileName(fileName)

    val packageName = getPackageName(ast)

    // Use config.get here, because we want to be able to see mistakes in configuration
    val ruleSet = config.get.determineRulesSet(packageName, name)

    val CompilationUnit(statements, _) = ast
    statements.immediateChildren
      .flatMap {
        case ImportClause(_, BlockImportExpr(prefix, ImportSelectors(_, first, others, _)), _, _) =>
          val basePackage = exprToText(prefix.contents)
          (first :: others.map(_._2)).flatMap {
            case s: Expr => Some((s.firstToken.offset, s"$basePackage${s.contents.head.tokens.head.text}"))
            case _       => None
          }
        case n @ ImportClause(_, Expr(contents), _, _) => Some((n.firstToken.offset, exprToText(contents)))
        case _                                         => Nil
      }
      .foldLeft((Map.empty[String, String], List.empty[PositionError])) {
        case (
              (scope, errors),
              (offset, importText @ importDecomposeRe(firstPart, middlePart, importedItem))
            ) =>
          // scope is necessary to allow situations like:
          //   import java.util
          //   import util.Collection
          // In that case first `java.util` is validated and then `java.util.Collection`
          val fullImport =
            scope
              .get(firstPart)
              .map(result => s"$result.${Option(middlePart).getOrElse("")}$importedItem")
              .getOrElse(importText)
          val newScope = if (importedItem == "_") scope else scope + (importedItem -> fullImport)
          val result = ruleSet.validateImport(fullImport)
          val maybeError =
            Some(PositionError(offset, args = List(fullImport, result.rule, result.packageDescription)))
              .filterNot(_ => result.allowed)
          (newScope, errors ::: maybeError.toList)
        case (result, _) => result // invalid import; ignore
      }
      ._2
  }

  private def extractFileName(fileName: String): String = {
    val fileNameWithoutExtension = {
      val lastDotPosition = fileName.lastIndexOf('.')
      if (lastDotPosition >= 0) fileName.take(lastDotPosition)
      else fileName
    }
    fileNameWithoutExtension.drop(
      fileNameWithoutExtension.lastIndexOf(java.io.File.separatorChar.toInt) + 1
    )
  }

  private def exprToText(contents: List[ExprElement]): String =
    contents
      .flatMap {
        case GeneralTokens(toks) => toks.map(_.text)
        case n: Any              => throw new IllegalStateException(s"FIXME: unexpected expr child node $n")
      }
      .mkString("")

  private def getPackageName(ast: CompilationUnit): String = {
    def isPartOfPackageName(t: Token): Boolean = (t.tokenType == DOT) || (t.tokenType == VARID)

    @annotation.tailrec
    def getNextPackageName(tokens: List[Token]): (List[Token], List[Token]) = tokens match {
      case Nil                                   => (Nil, Nil)
      case hd :: tail if hd.tokenType == PACKAGE => tail.span(isPartOfPackageName)
      case l: Any => getNextPackageName(l.dropWhile(tok => tok.tokenType != PACKAGE))
    }

    @annotation.tailrec
    def getPackageNameLoop(tokens: List[Token], myAccumulator: List[List[Token]]): List[List[Token]] =
      getNextPackageName(tokens) match {
        case (Nil, Nil) => myAccumulator.reverse // Return the result, but reverse since we gathered backward
        case (Nil, remainder) =>
          getPackageNameLoop(remainder, myAccumulator) // Found package object - try again
        case (l, remainder) => // add match to results, go look again
          val pkgName = l.filter(tok => tok.tokenType != DOT) // Strip out the dots between varids
          getPackageNameLoop(remainder, pkgName :: myAccumulator)
      }

    val packageNames = getPackageNameLoop(ast.tokens, Nil)
    packageNames.flatten.map(_.text).mkString(".")
  }
}

object ImportControlConfig {
  import ImportRule._
  import SourceIdentifier._
  import StrategyOnMismatch._

  def apply(source: InputSource): ImportControlConfig = {
    val xml = XML.load(source)
    readImportControlConfig(xml) match {
      case Right(config) => config
      case Left(msg)     => throw new RuntimeException(s"Configuration problem found: $msg")
    }
  }

  private def readImportControlConfig(elem: Elem): Either[String, ImportControlConfig] = {
    for {
      pkg <- elem.attribute("pkg").map(_.text).toRight("'pkg' attribute is required on 'import-control' node")
      _   <- validAttributes(elem, "pkg", "strategyOnMismatch", "regex")
      strategyOnMismatch <-
        readStrategyOnMismatch(elem, "disallowed")
          .filterOrElse(
            _ != DelegateToParent,
            "'DelegateToParent' not allowed as 'strategyOnMismatch' for 'import-control' node"
          )
      regex   <- Right[String, Boolean](readBoolean(elem, "regex"))
      rules   <- readImportRules(elem)
      sources <- readSources(elem)
    } yield ImportControlConfig(pkg, strategyOnMismatch, regex, rules, sources)
  }

  private def validAttributes(node: Node, attributes: String*): Either[String, Unit] = {
    node.attributes.collect {
      case attr: Attribute if !attributes.contains(attr.key) => attr.key
    } match {
      case Nil         => Right(())
      case head :: Nil => Left(s"Attribute '$head' is not valid on node '${node.label}'")
      case list =>
        Left(s"Attributes ${list.map(a => s"'$a'").mkString(", ")} are not valid on node '${node.label}'")
    }
  }

  private def readImportRules(node: Node): Either[String, List[ImportRule]] = {
    def readImportRule(node: Node): Either[String, ImportRule] = {
      val isPackageRule = node.attribute("pkg").isDefined
      for {
        name <- node
          .attribute("pkg")
          .orElse(node.attribute("class"))
          .map(_.text)
          .toRight(s"'pkg' or 'class' attribute required on '${node.label}' node")
        _ <-
          if (isPackageRule) validAttributes(node, "pkg", "exact-match", "local-only", "regex")
          else validAttributes(node, "class", "local-only", "regex")
      } yield {
        val exactMatch = readBoolean(node, "exact-match")
        val localOnly = readBoolean(node, "local-only")
        val regex = readBoolean(node, "regex")
        (node.label, isPackageRule) match {
          case ("allow", true)     => AllowPackage(name, exactMatch, localOnly, regex)
          case ("allow", false)    => AllowClass(name, localOnly, regex)
          case ("disallow", true)  => DisallowPackage(name, exactMatch, localOnly, regex)
          case ("disallow", false) => DisallowClass(name, localOnly, regex)
          case _                   => throw new RuntimeException("Impossible option")
        }
      }
    }

    node.child
      .filter(c => c.label == "allow" || c.label == "disallow")
      .map(readImportRule)
      .toSeq
      .sequence
  }

  private def readSources(node: Node): Either[String, List[SourceIdentifier]] = {
    def readSource(node: Node): Either[String, SourceIdentifier] =
      for {
        name <- node
          .attribute("name")
          .map(_.text)
          .toRight(s"'name' attribute required on '${node.label}' node")
        _ <-
          if (node.label == "file") validAttributes(node, "name", "regex")
          else validAttributes(node, "name", "strategyOnMismatch", "regex")
        strategyOnMismatch <- readStrategyOnMismatch(node, "delegateToParent")
        rules              <- readImportRules(node)
        sources            <- readSources(node)
      } yield {
        val regex = readBoolean(node, "regex")
        node.label match {
          case "subpackage" => Subpackage(name, strategyOnMismatch, regex, rules, sources)
          case "file"       => File(name, regex, rules)
        }
      }

    // first process file sources to have file matches precede subpackage sources
    ((node \ "file" map readSource) ++ (node \ "subpackage" map readSource)).sequence
  }

  private def readBoolean(node: Node, label: String): Boolean =
    node
      .attribute(label)
      .map(_.text)
      .collect {
        case "true"  => true
        case "false" => false
      }
      .getOrElse(true)

  implicit class SeqOfEitherOp[A, B](eithers: Seq[Either[A, B]]) {
    def sequence: Either[A, List[B]] =
      eithers.foldRight[Either[A, List[B]]](Right(List.empty[B])) {
        case (Right(b), acc) => acc.map(b :: _)
        case (Left(a), _)    => Left(a)
      }
  }

  implicit class EitherOps[A, B](either: Either[A, B]) {
    def map[C](fn: B => C): Either[A, C] = either.right.map(fn)
    def flatMap[C >: A, D](fn: B => Either[C, D]): Either[C, D] = either.right.flatMap(fn)
    def filterOrElse(predicate: B => Boolean, or: => A): Either[A, B] = either match {
      case Right(b) if predicate(b) => Right(b)
      case Right(_)                 => Left(or)
      case Left(a)                  => Left(a)
    }
  }

  private def readStrategyOnMismatch(node: Node, default: String): Either[String, StrategyOnMismatch] =
    node.attribute("strategyOnMismatch").map(_.text).getOrElse(default) match {
      case "delegateToParent" => Right(DelegateToParent)
      case "allowed"          => Right(Allowed)
      case "disallowed"       => Right(Disallowed)
      case other: String =>
        Left(s"Unsupported value '$other' for 'strategyOnMismatch' attribute on '${node.label}' node")
    }

  sealed trait RuleContainer {
    def rules: List[ImportRule]

    def strategyOnMismatch: StrategyOnMismatch

    override def toString: String = this match {
      case s: ImportControlConfig   => s"[root] '${s.pkg}'"
      case s: Subpackage            => s"[sub] '${s.name}'"
      case s: SourceIdentifier.File => s"[file] '${s.name}'"
    }
  }

  case class MatchResult(ruleContainer: RuleContainer, exactMatch: Boolean)

  case class ImportValidationResult(rule: String, packageDescription: String, allowed: Boolean)

  case class RuleSet(results: List[MatchResult]) {
    def validateImport(forImport: String): ImportValidationResult = {
      case class ImportValidationResultInternal(rule: ImportRule, allowed: Boolean)
      def checkAccess(result: MatchResult): Option[ImportValidationResultInternal] = {
        result.ruleContainer.rules
          .filter(rule => result.exactMatch || !rule.localOnly)
          .flatMap(rule => rule.verifyImport(forImport).map(ImportValidationResultInternal(rule, _)))
          .headOption
      }

      def describeLocation(remainingResults: List[MatchResult]): String =
        remainingResults.reverse.map(_.ruleContainer.toString).mkString("/")

      @tailrec
      def loop(remainingResults: List[MatchResult]): ImportValidationResult =
        remainingResults match {
          case Nil => ImportValidationResult("no match with root package", "n/a", allowed = false)
          case head :: tail =>
            checkAccess(head) match {
              case Some(ImportValidationResultInternal(rule, allowed)) =>
                ImportValidationResult(rule.toString, describeLocation(remainingResults), allowed)
              case None =>
                head.ruleContainer.strategyOnMismatch match {
                  case DelegateToParent => loop(tail)
                  case Disallowed =>
                    ImportValidationResult(
                      "mismatch strategy",
                      describeLocation(remainingResults),
                      allowed = false
                    )
                  case Allowed =>
                    ImportValidationResult(
                      "mismatch strategy",
                      describeLocation(remainingResults),
                      allowed = true
                    )
                }
            }
        }

      loop(results)
    }

  }

  sealed abstract class PackageMatcher(
    name: String,
    regex: Boolean,
    private[scalariform] val childSources: List[SourceIdentifier]
  ) extends RuleContainer {
    private val nameRegex = {
      val regexBase = if (regex) name else Regex.quote(name)
      (s"(?:$regexBase)(?:\\.(.*))?").r
    }

    def locateRules(packageName: String, fileName: String): Option[List[MatchResult]] =
      packageName match {
        case nameRegex(rest) =>
          val subpackage = Option(rest).getOrElse("")
          val thisResult = MatchResult(this, rest == null)
          Some(
            childSources
              .map(_.locateRules(subpackage, fileName))
              .collectFirst { case Some(list) => thisResult :: list }
              .getOrElse(List(thisResult))
          )
        case _ => None
      }
  }

  object SourceIdentifier {
    sealed trait SourceIdentifier extends RuleContainer {
      def locateRules(packageName: String, fileName: String): Option[List[MatchResult]]
    }

    case class Subpackage(
      name: String,
      strategyOnMismatch: StrategyOnMismatch = DelegateToParent,
      regex: Boolean                         = true,
      rules: List[ImportRule],
      sourceIdentifiers: List[SourceIdentifier]
    ) extends PackageMatcher(name, regex, sourceIdentifiers)
        with SourceIdentifier

    case class File(name: String, regex: Boolean = true, rules: List[ImportRule]) extends SourceIdentifier {
      private val nameRegex = {
        val regexBase = if (regex) name else Regex.quote(name)
        (s"(?:$regexBase)").r
      }

      override val strategyOnMismatch: StrategyOnMismatch = DelegateToParent

      override def locateRules(packageName: String, fileName: String): Option[List[MatchResult]] =
        Option(fileName)
          .filter(_ => packageName.isEmpty)
          .filter {
            case nameRegex() => true
            case _           => false
          }
          .map(_ => List(MatchResult(this, exactMatch = true)))
    }
  }

  object ImportRule {
    sealed trait ImportRule {
      val name: String

      val localOnly: Boolean

      val exactMatch: Boolean = true

      val regex: Boolean

      def verifyImport(forImport: String): Option[Boolean]

      override def toString: String = this match {
        case rule: AllowPackage    => s"allow rule for package '${rule.name}'"
        case rule: DisallowPackage => s"disallow rule for package '${rule.name}'"
        case rule: AllowClass      => s"allow rule for class '${rule.name}'"
        case rule: DisallowClass   => s"disallow rule for class '${rule.name}'"
      }
    }

    sealed protected abstract class PackageImportRule(allow: Boolean) extends ImportRule {
      private val nameRegex = {
        val regexBase = if (regex) name else Regex.quote(name)
        (s"(?:$regexBase)(?:\\..*)?(\\..*)?").r
      }

      private def importMatch(forImport: String): Boolean =
        forImport match {
          case nameRegex(lastSegment) => !exactMatch || lastSegment == null
          case _                      => false
        }

      override def verifyImport(forImport: String): Option[Boolean] =
        if (importMatch(forImport)) Some(allow) else None
    }

    sealed protected abstract class ClassImportRule(allow: Boolean) extends ImportRule {
      private val nameRegex = {
        val regexBase = if (regex) name else Regex.quote(name)
        (s"(?:$regexBase)").r
      }

      private def importMatch(forImport: String): Boolean =
        forImport match {
          case nameRegex() => true
          case _           => false
        }

      override def verifyImport(forImport: String): Option[Boolean] =
        if (importMatch(forImport)) Some(allow) else None
    }

    case class AllowPackage(
      name: String,
      override val exactMatch: Boolean = true,
      localOnly: Boolean               = true,
      regex: Boolean                   = true
    ) extends PackageImportRule(allow = true)

    case class AllowClass(name: String, localOnly: Boolean = true, regex: Boolean = true)
        extends ClassImportRule(allow = true)

    case class DisallowPackage(
      name: String,
      override val exactMatch: Boolean = true,
      localOnly: Boolean               = true,
      regex: Boolean                   = true
    ) extends PackageImportRule(allow = false)

    case class DisallowClass(name: String, localOnly: Boolean = true, regex: Boolean = true)
        extends ClassImportRule(allow = false)
  }

  object StrategyOnMismatch {
    sealed trait StrategyOnMismatch

    case object DelegateToParent extends StrategyOnMismatch

    case object Allowed extends StrategyOnMismatch

    case object Disallowed extends StrategyOnMismatch
  }

  case class ImportControlConfig(
    pkg: String,
    strategyOnMismatch: StrategyOnMismatch = Disallowed,
    regex: Boolean                         = true,
    rules: List[ImportRule],
    sourceIdentifiers: List[SourceIdentifier]
  ) extends PackageMatcher(pkg, regex, sourceIdentifiers) {

    def determineRulesSet(packageName: String, fileName: String): RuleSet =
      RuleSet(locateRules(packageName, fileName).getOrElse(Nil).reverse)
  }
}
