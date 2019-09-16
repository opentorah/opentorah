package org.digitaljudaica.archive.collector

import scala.xml.{Elem, Node}
import Xml.Ops
import Names.Named

import scala.xml.transform.{RewriteRule, RuleTransformer}

final class Names(layout: Layout) extends DocumentLike(layout.namesFileDirectory, layout.namesFileName) {
  override def url: String = layout.namesUrl

  private val named2name: Map[String, String] = Map(
    "person" -> "persName",
    "place" -> "placeName",
    "org" -> "orgName"
  )

  val (persons: Seq[Named], persNames: Seq[Name]) = namedAndNames("person")
  val (places: Seq[Named], placeNames: Seq[Name]) = namedAndNames("place")
  val (orgs: Seq[Named], orgNames: Seq[Name]) = namedAndNames("org")

  private def namedAndNames(namedElementName: String): (Seq[Named], Seq[Name]) = {
    val nameElementName: String = named2name(namedElementName)
    def namedNameElems(elem: Elem): Seq[Elem] = elem.elemsFilter(nameElementName)
    val namedElems: Seq[Elem] = teiDescendants(namedElementName)
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

  private def addReferenced(references: Seq[Name]): Unit = {
    val resolvable: Seq[Name] = references.filter(_.isResolvable)

    val rule: RewriteRule = new RewriteRule {
      override def transform(node: Node): Seq[Node] = node match {
        case namedElem: Elem if named2name.keySet.contains(namedElem.label) =>
          val id: Option[String] = namedElem.attributeOption("xml:id")
          val mentionsElem: Elem = <p rendition="mentions">
            {for (ref <- Util.removeConsecutiveDuplicates(resolvable.filter(_.ref == id).map(_.document)))
              yield <ref target={ref.url} role="documentViewer">{ref.fileName}</ref>}</p>

          val (nonMentions, tail) = namedElem.child.span( _ match {
              case elem: Elem if elem.label == "p" && (elem \ "@rendition").text == "mentions" => false
              case _ => true
            }
          )

          val newChildren: Seq[Node] =
            if (tail.nonEmpty) nonMentions ++ Seq(mentionsElem) ++ tail.tail else  {
              val (names, tail) = namedElem.child.span(_ match {
                case elem: Elem => elem.label == named2name(namedElem.label)
                case _ => false
              })
              names ++ Seq(mentionsElem) ++ tail
            }

          namedElem.copy(child = newChildren)
        case other => other
      }
    }

    write(new RuleTransformer(rule).transform(tei).head.asInstanceOf[Elem])
  }

  def processReferences(documentReferences: Seq[Name]): Option[Seq[String]] = {
    val references: Seq[Name] = names ++ documentReferences
    addReferenced(references)

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
