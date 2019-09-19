package org.digitaljudaica.archive.collector

import scala.xml.{Elem, Node}
import Xml.Ops
import Names.Named

final class Names(layout: Layout) extends DocumentLike(layout.namesFileDirectory, layout.namesFileName) {
  override def url: String = layout.namesUrl

  val (persons: Seq[Named], persNames: Seq[Name]) = namedAndNames(Entity.Person)
  val (places: Seq[Named], placeNames: Seq[Name]) = namedAndNames(Entity.Place)
  val (orgs: Seq[Named], orgNames: Seq[Name]) = namedAndNames(Entity.Organization)

  private def namedAndNames(entity: Entity): (Seq[Named], Seq[Name]) = {
    val nameElementName: String = entity.nameElement
    def namedNameElems(elem: Elem): Seq[Elem] = elem.elemsFilter(nameElementName)
    val namedElems: Seq[Elem] = teiDescendants(entity.element)
    val exclude: Set[Elem] = namedElems.flatMap(namedNameElems).toSet
    (
      namedElems.map(elem => new Named(
        id = elem.attributeOption("xml:id"),
        names = names(namedNameElems(elem))
      )),
      names(teiDescendants(nameElementName).filterNot(exclude.contains))
    )
  }

  private val nameds: Seq[Named] = (persons ++ places ++ orgs).filter(_.id.isDefined)

  private def isUnresolved(name: Name): Boolean = find(name.ref.get).isEmpty
  private def find(id: String): Option[Named] = nameds.find(_.id.contains(id))

  private def rewriteNamedElement(references: Seq[Name])(namedElem: Elem): Elem = {
    Entity.forElement(namedElem.label).fold(namedElem) { entity =>
      val (nonMentions, tail) = namedElem.child.span(_ match {
        case elem: Elem if elem.label == "p" && (elem \ "@rendition").text == "mentions" => false
        case _ => true
      })

      val (before, after) = if (tail.nonEmpty) (nonMentions, tail.tail) else namedElem.child.span(_ match {
        case elem: Elem => elem.label == entity.nameElement
        case _ => false
      })

      namedElem.copy(child = before ++ Seq(mentions(references, namedElem)) ++ after)
    }
  }

  private def mentions(references: Seq[Name], namedElem: Elem): Elem = {
    val id: Option[String] = namedElem.attributeOption("xml:id")
    <p rendition="mentions">
      {for (ref <- Util.removeConsecutiveDuplicates(references.filter(_.ref == id).map(_.document)))
        yield <ref target={ref.url} role="documentViewer">{ref.fileName}</ref>}
    </p>
  }

  def processReferences(documentReferences: Seq[Name]): Option[Seq[String]] = {
    val references: Seq[Name] = names ++ documentReferences
    val resolvable: Seq[Name] = references.filter(_.isResolvable)
    write(Xml.rewriteElements(tei, rewriteNamedElement(resolvable)))

    // Wrapper
    Util.writeTeiYaml(directory, fileName,
      layout = "names",
      tei = "names.xml",
      title = "Имена",
      target = "namesViewer"
    )

    def section(name: String, references: Seq[Name]): Seq[String] =
      if (references.isEmpty) Seq.empty
      else s"## $name references ##" +: (for (reference <- references) yield s" - ${reference.display}")

    val missing: Seq[Name] = references.filter(_.isMissing).filterNot(_.name == "?")
    val malformed: Seq[Name] = references.filter(_.isMalformed)
    val unresolved: Seq[Name] = references.filter(_.isResolvable).filter(isUnresolved)

    if (missing.isEmpty && malformed.isEmpty && unresolved.isEmpty) None else Some(
      section("Missing", missing) ++
        section("Malformed", malformed) ++
        section("Unresolved", unresolved))
  }
}

object Names {
  final class Named(
    val id: Option[String],
    val names: Seq[Name]
  )
}
