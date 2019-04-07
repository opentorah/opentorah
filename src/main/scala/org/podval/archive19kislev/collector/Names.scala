package org.podval.archive19kislev.collector

import java.io.File

import scala.xml.{Elem, Node}
import Xml.Ops
import Names.Named

import scala.xml.transform.{RewriteRule, RuleTransformer}

final class Names(directory: File, fileName: String) extends DocumentLike(directory, fileName) {
  override def url: String = "/" + fileName + ".html"

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

  val nameds: Seq[Named] = (persons ++ places ++ orgs).filter(_.id.isDefined)

  def find(id: String): Option[Named] = nameds.find(_.id.contains(id))

  def addReferenced(references: Seq[Name]): Unit = {
    val resolvable: Seq[Name] = references.filter(_.isResolvable)

    val rule: RewriteRule = new RewriteRule {
      override def transform(node: Node): Seq[Node] = node match {
        case namedElem: Elem if named2name.keySet.contains(namedElem.label) =>
          val id: Option[String] = namedElem.attributeOption("xml:id")
          val mentionsElem: Elem = <p rendering="mentions">
            {for (ref <- Util.removeConsecutiveDuplicates(resolvable.filter(_.ref == id).map(_.document)))
              yield <ref target={ref.url}>{ref.fileName}</ref>}</p>

          val (nonMentions, tail) = namedElem.child.span( _ match {
              case elem: Elem if elem.label == "p" && (elem \ "@rendering").text == "mentions" => false
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
}

object Names {
  final class Named(
    val id: Option[String],
    val names: Seq[Name]
  )
}
