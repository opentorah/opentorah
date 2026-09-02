package org.opentorah.metadata

import org.opentorah.util.{ClassName, Collections, Effects}
import org.podval.xml.{XmlAst, XmlCodec, XmlError, XmlParser}

final case class Names(names: Seq[Name]) extends Language.ToString:
  Collections.checkNoDuplicates(names.map(_.name), "names")
  // There may be multiple names for the same language (for an example, see Language),
  // so this check is disabled:
  // Collections.checkNoDuplicates(names.map(_.copy(name = "")), "name parameters")

  def isEmpty: Boolean = names.isEmpty

  def getDefaultName: Option[String] =
    if (names.length == 1) && names.head.languageSpec.isEmpty then Some(names.head.name) else None

  def find(name: String): Option[Name] = names.find(_.name == name)

  def hasName(name: String): Boolean = find(name).isDefined

  def find(spec: Language.Spec): Option[Name] = names.find(_.satisfies(spec))

  def doFind(spec: Language.Spec): Name =
    find(spec)
      .orElse(find(spec.dropFlavour))
      .orElse(find(spec.dropFlavour.dropIsTransliterated))
      .orElse(find(spec.dropFlavour.dropIsTransliterated.dropLanguage))
      .get

  def name: String = doFind(Language.Spec.empty).name

  override def toLanguageString(using spec: Language.Spec): String = doFind(spec).name

  def isDisjoint(other: Names): Boolean = names.forall(name => !other.hasName(name.name))

object Names:
  // TODO create a mix-in for Named that overrides names as a val with this as a value?
  def apply(name: String): Names = Names(Seq(Name(name, Language.Spec.empty)))

  /** Parent `n` as the default name, plus child `<name>` elements (Selector / Alias). */
  def fromDefaultName(n: Option[String], names: Seq[Name]): Names =
    val merged: Seq[Name] = mergeDefaultName(n, names)
    if merged.isEmpty then throw XmlError("No names and no default name")
    Names(merged)

  private def mergeDefaultName(n: Option[String], names: Seq[Name]): Seq[Name] =
    val defaultName: Option[Name] = n.map(_.trim).filter(_.nonEmpty).map(Name(_, Language.Spec.empty))
    if names.isEmpty then defaultName.toSeq else
      defaultName.fold(names)(defaultName =>
        if names.exists(_.name == defaultName.name) then names else names :+ defaultName
      )

  val codec: XmlCodec[Names] = new XmlCodec[Names]:
    override def elementName: String = "names"
    override def isRecordLike: Boolean = true

    override def unsafeDecode[E: XmlAst](element: E): Names =
      Names(Name.codec.decodeChildren(element).fold(error => throw error, identity))

    override def encodeNamed[E: XmlAst](elName: String, value: Names): E =
      val ast: XmlAst[E] = summon[XmlAst[E]]
      ast.element(elName, Seq.empty, value.names.map(name => Name.codec.encode(name)))

  def checkDisjoint(nameses: Seq[Names]): Unit =
    for
      one: Names <- nameses
      other: Names <- nameses if !other.eq(one)
    yield
      require(one.isDisjoint(other), s"Names overlap: $one and $other")

  // TODO If I ever figure out how to work with Custom using Cats typeclasses, something similar
  // should work here too :)
  def combine(one: Names, other: Names, combiner: (Language.Spec, String, String) => String): Names =
    val specs: Set[Language.Spec] = one.names.map(_.languageSpec).toSet ++ other.names.map(_.languageSpec)
    val result: Set[Name] = specs.map(spec =>
      Name(combiner(spec, one.doFind(spec).name, other.doFind(spec).name), spec))
    new Names(result.toSeq)

  abstract class Loader[Key <: HasName](resourceNameOverride: Option[String] = None) extends HasValues[Key]:
    // This is lazy to allow correct initialization:
    // Language metadata file references Language instances by name :)
    final lazy val toNames: Map[Key, Names] =
      val resourceName: String = resourceNameOverride.getOrElse(ClassName.get(this))
      val nameses: Seq[Names] = XmlParser.parseCatalog(
        getClass,
        s"$resourceName.xml",
        resourceName,
        Names.codec
      ).fold(error => throw error, identity)
      Effects.unsafeRun(HasName.mapByName(
        keys = valuesSeq,
        metadatas = nameses,
        hasName = (metadata: Names, name: String) => metadata.hasName(name)
      ))
