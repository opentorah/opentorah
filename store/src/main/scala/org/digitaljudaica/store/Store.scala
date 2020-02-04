package org.digitaljudaica.store

import java.io.File
import org.digitaljudaica.store.metadata.XmlParser.{Parser, attribute, optionalAttribute, optionalCharacters,
  element, elements, optionalElement}
import org.digitaljudaica.store.metadata.{Language, LanguageSpec, Name, Names, Xml, XmlParser}
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
    if (textCache.isEmpty) textCache = Some(Xml.load(new File(url), s"$name.xml"))
    textCache.get
  }

  override def names: Names = ??? // TODO based on name
  override def by: Option[By] = None // TODO internal structure of the text (pages, chapters...)
}

object Store {

  val languageSpecParser: Parser[LanguageSpec] = for {
    lang <- optionalAttribute("lang")
    transliterated <- optionalAttribute("transliterated")
    flavour <- optionalAttribute("flavour")
  } yield LanguageSpec(
    language = lang.map(Language.getForDefaultName),
    isTransliterated = XmlParser.getBoolean(transliterated),
    flavour = flavour
  )

  val nameParser: Parser[Name] = for {
    n <- optionalAttribute("n")
    characters <- optionalCharacters
    name = n.orElse(characters)
    _ <- XmlParser.addError(n.isEmpty && characters.isEmpty, "Both 'n' attribute and text are absent.")
    _ <- XmlParser.addError(n.nonEmpty && characters.nonEmpty, "Both 'n' attribute and text are present.")
    languageSpec <- languageSpecParser
  } yield Name(name.getOrElse(""), languageSpec)

  val namesParser: Parser[Names] = for {
    names <- elements("name", nameParser)
  } yield new Names(names)

  val selectorParser: Parser[Selector] = for {
    names <- namesParser
  } yield Selector(names)

  def storeParser(inheritedSelectors: Set[Selector]): Parser[Store] = for {
    names <- namesParser
    selectors <- elements("selector", selectorParser)
    by <- element("by", byParser(inheritedSelectors ++ selectors.toSet))
  } yield new BaseStore(
    names,
    selectors,
    by
  )

  val textsParser: Parser[String] = for {
    url <- attribute("url")
  } yield url

  // TODO allow 'by' to be named in-line
  def byParser(selectors: Set[Selector]): Parser[By] = for {
    n <- attribute("n")
    selector = selectorByName(selectors, n)
    texts <- optionalElement("texts", textsParser)
    stores <- elements("store", storeParser(selectors))
    _ <- XmlParser.addError(texts.isEmpty && stores.isEmpty, "Both 'stores' and 'texts' elements are absent.")
    _ <- XmlParser.addError(texts.nonEmpty && stores.nonEmpty, "Both 'stores' and 'texts' elements are present.")
  } yield {
    if (selector.isEmpty) throw new IllegalArgumentException(s"Selector not found: $n")
    if (texts.isDefined) new TextsBy(selector.get, texts.get) else new BaseBy(selector.get, stores)
  }

  def selectorByName(selectors: Set[Selector], name: String): Option[Selector] = {
    val result = selectors.find(_.names.hasName(name))
    result
  }

  def main(args: Array[String]): Unit = {
    val xml: Elem = Xml.load(new File("docs").getAbsoluteFile, "store")
    val store = XmlParser.runToFinish(xml, "store", storeParser(Set.empty))
    val y = 0
  }
}
