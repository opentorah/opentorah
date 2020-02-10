package org.digitaljudaica.store

import cats.implicits._
import java.io.File
import org.digitaljudaica.metadata.Names
import org.digitaljudaica.xml.Parse
import org.digitaljudaica.xml.Parse.{Parser, requiredAttribute, check, checkName, element, elements, optionalElement}
import org.digitaljudaica.xml.Load
import scala.xml.Elem

sealed trait Store {
  def names: Names
  def by: Option[By]
}

final class BaseStore(
  override val names: Names,
  selectors: Seq[Selector],
  baseBy: By
) extends Store {
  override def by: Option[By] = Some(baseBy)
}

final class TextStore(
  name: String,
  url: String
) extends Store {
  private var textCache: Option[Elem] = None

  def text: Elem = {
    if (textCache.isEmpty) textCache = Some(Load.fromFileDo(new File(url), s"$name.xml"))
    textCache.get
  }

  override def names: Names = ??? // TODO based on name
  override def by: Option[By] = None // TODO internal structure of the text (pages, chapters...)
}

object Store {

  val selectorParser: Parser[Selector] = for {
    names <- Names.parser // TODO handle default name
  } yield Selector(names)

  def storeParser(inheritedSelectors: Set[Selector]): Parser[Store] = checkName("store", for {
    names <- Names.parser // TODO handle default name
    selectors <- elements("selector", selectorParser)
    by <- element("by", byParser(inheritedSelectors ++ selectors.toSet))
  } yield new BaseStore(
    names,
    selectors,
    by
  ))

  val textsParser: Parser[String] = for {
    url <- requiredAttribute("url")
  } yield url

  // TODO allow 'by' to be named in-line
  def byParser(selectors: Set[Selector]): Parser[By] = for {
    n <- requiredAttribute("n")
    selector = selectorByName(selectors, n)
    texts <- optionalElement("texts", textsParser)
    stores <- elements("store", storeParser(selectors))
    _ <- check(texts.nonEmpty || stores.nonEmpty, "Both 'stores' and 'texts' elements are absent.")
    _ <- check(texts.isEmpty || stores.isEmpty, "Both 'stores' and 'texts' elements are present.")
  } yield {
    if (selector.isEmpty) throw new IllegalArgumentException(s"Selector not found: $n")
    if (texts.isDefined) new TextsBy(selector.get, texts.get) else new BaseBy(selector.get, stores)
  }

  def selectorByName(selectors: Set[Selector], name: String): Option[Selector] = {
    val result = selectors.find(_.names.hasName(name))
    result
  }

  def main(args: Array[String]): Unit = {
    val xml = Load.fromFile(new File("docs").getAbsoluteFile, "store")
    val store = Parse.parse(xml, storeParser(Set.empty))
    val y = 0
  }
}
