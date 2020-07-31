package org.opentorah.metadata

import org.opentorah.xml.{Attribute, Parser}
import org.opentorah.util.Collections
import scala.xml.Elem
import zio.ZIO

final class Names(val names: Seq[Name]) extends LanguageString {
  Collections.checkNoDuplicates(names.map(_.name), "names")
  // There may be multiple names for the same language (for an example, see Language),
  // so this check is disabled:
  // Collections.checkNoDuplicates(names.map(_.copy(name = "")), "name parameters")

  def isEmpty: Boolean = names.isEmpty

  def getDefaultName: Option[String] =
    if ((names.length == 1) && (names.head.languageSpec == LanguageSpec.empty)) Some(names.head.name) else None

  def find(name: String): Option[Name] = names.find(_.name == name)

  def hasName(name: String): Boolean = find(name).isDefined

  def find(spec: LanguageSpec): Option[Name] = names.find(_.satisfies(spec))

  def doFind(spec: LanguageSpec): Name =
    find(spec)
      .orElse(find(spec.dropFlavour))
      .orElse(find(spec.dropFlavour.dropIsTransliterated))
      .orElse(find(spec.dropFlavour.dropIsTransliterated.dropLanguage))
      .get

  def name: String = doFind(LanguageSpec.empty).name

  def toLanguageString(implicit spec: LanguageSpec): String = doFind(spec).name

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

  final val defaultNameAttribute: Attribute[String] = Attribute("n")

  val withoutDefaultNameParser: Parser[Names] = parser(isDefaultNameAllowed = false)

  val withDefaultNameParser: Parser[Names] = parser(isDefaultNameAllowed = true)

  def parser(isDefaultNameAllowed: Boolean): Parser[Names] = for {
    n <- if (!isDefaultNameAllowed) ZIO.none else defaultNameAttribute.optional
    defaultName = n.map(Name(_, LanguageSpec.empty))
    nonDefaultNames <- Name.all
    _ <- Parser.check(nonDefaultNames.nonEmpty || defaultName.isDefined, s"No names and no default name")
  } yield {
    val names = if (nonDefaultNames.isEmpty) Seq(defaultName.get) else
      defaultName.fold(nonDefaultNames){ defaultName =>
      if (nonDefaultNames.exists(_.name == defaultName.name)) nonDefaultNames
      else nonDefaultNames :+ defaultName
    }

    new Names(names)
  }

  def attributes(value: Names): Seq[Attribute.Value[_]] =
    Seq(defaultNameAttribute.withValue(value.getDefaultName))

  def content(value: Names): Seq[Elem] =
    if (value.getDefaultName.isDefined) Seq.empty else toXml(value)

  def toXml(value: Names): Seq[Elem] =
    Name.toXml(value.names)
}
