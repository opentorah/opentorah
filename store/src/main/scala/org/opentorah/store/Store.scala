package org.opentorah.store

import java.io.File
import org.opentorah.metadata.Names
import org.opentorah.xml.{Element, From, Parser, Xml}
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
    if (textCache.isEmpty) textCache = Some(Parser.run(From.file(new File(url), s"$name.xml").load))
    textCache.get
  }

  override def names: Names = ??? // TODO based on name
  override def by: Option[By] = None // TODO internal structure of the text (pages, chapters...)
}

object Store {

  def parser(inheritedSelectors: Set[Selector]): Parser[Store] = Xml.withName("store", for {
    names <- Names.withDefaultNameParser
    selectors <- Element("selector",  Selector.parser).all
    _ = println(names)
    _ = println(selectors)
    by <- Element("by", byParser(inheritedSelectors ++ selectors.toSet)).required
  } yield new BaseStore(
    names,
    selectors,
    by
  ))

  val textsParser: Parser[String] = for {
    url <- Xml.attribute.required("url")
  } yield url

  // TODO allow 'by' to be named in-line
  def byParser(selectors: Set[Selector]): Parser[By] = for {
    n <- Xml.attribute.required("n")
    selector = selectorByName(selectors, n)
    texts <- Element("texts", textsParser).optional
    stores <- Element("store", parser(selectors)).all
    _ <- Parser.check(texts.nonEmpty || stores.nonEmpty, "Both 'stores' and 'texts' elements are absent.")
    _ <- Parser.check(texts.isEmpty || stores.isEmpty, "Both 'stores' and 'texts' elements are present.")
  } yield {
    if (selector.isEmpty) throw new IllegalArgumentException(s"Selector not found: $n")
    if (texts.isDefined) new TextsBy(selector.get, texts.get) else new BaseBy(selector.get, stores)
  }

  def selectorByName(selectors: Set[Selector], name: String): Option[Selector] = {
    val result = selectors.find(_.names.hasName(name))
    result
  }
}
