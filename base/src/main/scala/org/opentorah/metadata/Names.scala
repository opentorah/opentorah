package org.opentorah.metadata

import org.opentorah.util.{Collections, Effects}
import org.opentorah.xml.{Attribute, Element, Parsable, Parser, Unparser}
import zio.ZIO

final class Names(val names: Seq[Name]) extends Language.ToString:
  Collections.checkNoDuplicates(names.map(_.name), "names")
  // There may be multiple names for the same language (for an example, see Language),
  // so this check is disabled:
  // Collections.checkNoDuplicates(names.map(_.copy(name = "")), "name parameters")

  def isEmpty: Boolean = names.isEmpty

  def getDefaultName: Option[String] =
    if (names.length == 1) && (names.head.languageSpec == LanguageSpec.empty) then Some(names.head.name) else None

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

  override def toLanguageString(using spec: LanguageSpec): String = doFind(spec).name

  def isDisjoint(other: Names): Boolean = names.forall(name => !other.hasName(name.name))

  def withNumber(number: Int): Names = transform(_.withNumber(number))
  
  def withNumbers(from: Int, to: Int): Names = transform(_.withNumbers(from, to))

  def transform(transformer: Name => Name): Names = new Names(names.map(transformer))

object Names:
  // TODO create a mix-in for Named that overrides names as a val with this as a value?
  def apply(name: String): Names = new Names(Seq(Name(name, LanguageSpec.empty)))

  def checkDisjoint(nameses: Seq[Names]): Unit =
    for
      one: Names <- nameses
      other: Names <- nameses if !other.eq(one)
    yield
      require(one.isDisjoint(other), s"Names overlap: $one and $other")

  // TODO If I ever figure out how to work with Custom using Cats typeclasses, something similar
  // should work here too :)
  def combine(one: Names, other: Names, combiner: (LanguageSpec, String, String) => String): Names =
    val specs: Set[LanguageSpec] = one.names.map(_.languageSpec).toSet ++ other.names.map(_.languageSpec)
    val result: Set[Name] = specs.map(spec =>
      Name(combiner(spec, one.doFind(spec).name, other.doFind(spec).name), spec))
    new Names(result.toSeq)

  final val defaultNameAttribute: Attribute[String] = Attribute("n")

  def parser(isDefaultNameAllowed: Boolean): Parser[Names] = for
    n <- if !isDefaultNameAllowed then ZIO.none else defaultNameAttribute.optional()
    defaultName = n.map(Name(_, LanguageSpec.empty))
    nonDefaultNames <- Name.seq()
    _ <- Effects.check(nonDefaultNames.nonEmpty || defaultName.isDefined, s"No names and no default name")
  yield
    val names = if nonDefaultNames.isEmpty then Seq(defaultName.get) else
      defaultName.fold(nonDefaultNames)(defaultName =>
      if nonDefaultNames.exists(_.name == defaultName.name) then nonDefaultNames else nonDefaultNames :+ defaultName
    )
    new Names(names)

  val withDefaultNameParsable: Parsable[Names] = new Parsable[Names]:
    override protected def parser: Parser[Names] = Names.this.parser(isDefaultNameAllowed = true)
    override def unparser: Unparser[Names] = Unparser(
      attributes = value => Seq(defaultNameAttribute.optional.withValue(value.getDefaultName)),
      content = value => if value.getDefaultName.isDefined then Seq.empty else Name.seq[Names](_.names).content(value) // TODO ???
    )

  val withoutDefaultNameParsable: Parsable[Names] = new Parsable[Names]:
    override protected def parser: Parser[Names] = Names.this.parser(isDefaultNameAllowed = false)
    override def unparser: Unparser[Names] = Name.seq[Names](_.names)

  object NamesMetadata extends Element[Names]("names"):
    override def contentParsable: Parsable[Names] = withoutDefaultNameParsable
