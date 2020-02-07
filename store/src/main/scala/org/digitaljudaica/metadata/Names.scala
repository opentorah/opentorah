package org.digitaljudaica.metadata

import cats.implicits._
import org.digitaljudaica.metadata.Xml.{Parser, elements}
import org.digitaljudaica.util.Collections
import scala.xml.Elem

final class Names(val names: Seq[Name]) extends HasName with LanguageString {
  Collections.checkNoDuplicates(names.map(_.name), "names")
  // There may be multiple names for the same language (for an example, see Language),
  // so this check is disabled:
  // Util.checkNoDuplicates(names.map(_.copy(name = "")), "name parameters")

  def isEmpty: Boolean = names.isEmpty

  def find(name: String): Option[Name] = names.find(_.name == name)

  def hasName(name: String): Boolean = find(name).isDefined

  def find(spec: LanguageSpec): Option[Name] = names.find(_.satisfies(spec))

  def doFind(spec: LanguageSpec): Name =
    find(spec)
      .orElse(find(spec.dropFlavour))
      .orElse(find(spec.dropFlavour.dropIsTransliterated))
      .orElse(find(spec.dropFlavour.dropIsTransliterated.dropLanguage))
      .get

  def toLanguageString(implicit spec: LanguageSpec): String = doFind(spec).name.toString

  def isDisjoint(other: Names): Boolean = names.forall(name => !other.hasName(name.name))

  def withNumber(number: Int): Names =
    transform(name => name.copy(name.name + " " + name.languageSpec.toString(number)))

  def transform(transformer: Name => Name): Names = new Names(names.map(transformer))
}

object Names {
  def checkDisjoint(nameses: Seq[Names]): Unit = {
    for {
      one <- nameses
      other <- nameses if !other.eq(one)
    } yield {
      require(one.isDisjoint(other), s"Names overlap: $one and $other")
    }
  }

  // If I ever figure out how work with Custom using Cats typeclasses, something similar
  // should work here too :)
  def combine(one: Names, other: Names, combiner: (LanguageSpec, String, String) => String): Names = {
    val specs: Set[LanguageSpec] = one.names.map(_.languageSpec).toSet ++ other.names.map(_.languageSpec)
    val result: Set[Name] = specs.map { spec =>
      Name(combiner(spec, one.doFind(spec).name, other.doFind(spec).name), spec) }
    new Names(result.toSeq)
  }

  // TODO switch to Parser[A]
  def parse(attributes: Attributes, elements: Seq[Elem]): (Names, Seq[Elem]) = {
    val defaultName: Option[Name] =
      attributes.get("n").map { defaultName => Name(defaultName, LanguageSpec.empty) }
    val (nameElements, tail) = Xml.take(elements, "name")
    val names: Names =
      if (nameElements.isEmpty) new Names(Seq(defaultName.get))
      else parse(nameElements, defaultName)

    (names, tail)
  }

  // TODO switch to Parser[A]
  def parse(elements: Seq[Elem]): (Names, Seq[Elem]) = {
    val (nameElements, tail) = Xml.take(elements, "name")
    (parse(nameElements, None), tail)
  }

  // TODO switch to Parser[A]
  def parse(element: Elem): Names = {
    val (attributes, nameElements) = Xml.open(element, "names")
    attributes.close()
    parse(nameElements, None)
  }

  // TODO switch to Parser[A]
  private def parse(nameElements: Seq[Elem], defaultName: Option[Name]): Names = {
    val nonDefaultNames: Seq[Name] = nameElements.map(element => Xml.runA(element, "name", Name.parser))
    val names = defaultName.fold(nonDefaultNames){ defaultName =>
      if (nonDefaultNames.exists(_.name == defaultName.name)) nonDefaultNames
      else nonDefaultNames :+ defaultName
    }
    new Names(names)
  }

  val parser: Parser[Names] = for {
    names <- elements("name", Name.parser)
  } yield new Names(names)
}
