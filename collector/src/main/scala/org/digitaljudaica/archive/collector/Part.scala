package org.digitaljudaica.archive.collector

import org.digitaljudaica.xml.{ContentType, Element, Parser, Xml}
import scala.xml.Node

final class Part(val title: Option[Seq[Node]], val documents: Seq[Document])

object Part {

  sealed abstract class Descriptor(title: Seq[Node]) {
    final def getTitle: Seq[Node] = title
  }

  object Descriptor {

    final case class Names(names: Seq[String], title: Seq[Node]) extends Descriptor(title)

    final case class From(name: String, title: Seq[Node]) extends Descriptor(title)

    final case class CatchAll(title: Seq[Node]) extends Descriptor(title)

    val parser: Parser[Descriptor] = for {
      from <- Xml.attribute.optional("from")
      names <- Element("document", Xml.text.required).all
      title <- Element("title", ContentType.Mixed, Xml.allNodes).required
    } yield {
      if (names.isEmpty) {
        if (from.isEmpty) CatchAll(title) else From(from.get, title)
      } else {
        if (from.isDefined) throw new IllegalArgumentException("Both document names and from are specified")
        Names(names, title)
      }
    }

    @scala.annotation.tailrec
    def forNames(descriptors: Seq[Descriptor], documents: Seq[Document], result: Map[Descriptor, Seq[Document]]):
    (Map[Descriptor, Seq[Document]], Seq[Document]) = descriptors match {
      case Nil => (result, documents)

      case (d@Names(names, _)) :: ds => forNames(
        ds,
        documents.filterNot(document =>  names.contains(document.name)),
        result.updated(d, names.map { documentName: String => documents.find(_.name == documentName).getOrElse {
          throw new IllegalArgumentException(s"Document $documentName missing - or already claimed.")
        }})
      )

      case _ :: ds => forNames(ds, documents, result)
    }

    @scala.annotation.tailrec
    def forFromAndCatchAll(descriptors: Seq[Descriptor], documents: Seq[Document], result: Map[Descriptor, Seq[Document]]):
    (Map[Descriptor, Seq[Document]], Seq[Document]) = descriptors match {
      case Nil => (result, documents)

      case Names(_, _) :: ds => forFromAndCatchAll(ds, documents, result)

      case (d@CatchAll(_)) :: ds =>
        checkNotEmpty(documents)
        forFromAndCatchAll(ds, Seq.empty, result.updated(d, documents))

      case (d1@From(f1, _)) :: d2 :: ds => d2 match {
        case Names(_, _) => forFromAndCatchAll(d1 :: ds, documents, result)

        case CatchAll(_) => throw new IllegalArgumentException("CatchAll part after a From one")

        case From(f2, _) =>
          checkNotEmpty(documents)
          checkFrom(documents, f1)
          val (partDocuments: Seq[Document], tail: Seq[Document]) = documents.span(_.name != f2)
          forFromAndCatchAll(d2 :: ds, tail, result.updated(d1, partDocuments))
      }

      case (d1@From(f1, _)) :: Nil =>
        checkNotEmpty(documents)
        checkFrom(documents, f1)
        (result.updated(d1, documents), Seq.empty)
    }

    private def checkNotEmpty(documents: Seq[Document]): Unit =
      if (documents.isEmpty) throw new IllegalArgumentException("No more documents!")

    private def checkFrom(documents: Seq[Document], from: String): Unit =
      if (documents.head.name != from) throw new IllegalArgumentException("Incorrect From document")

    def splitParts(descriptors: Seq[Descriptor], documents: Seq[Document]): Seq[Part] =
      if (descriptors.isEmpty) Seq(new Part(None, documents)) else {
        val (names2documents: Map[Descriptor, Seq[Document]], afterNames: Seq[Document]) =
          forNames(descriptors, documents, Map.empty)

        val (fromAndCatchAll2documents: Map[Descriptor, Seq[Document]], leftOver: Seq[Document]) =
          forFromAndCatchAll(descriptors, afterNames, Map.empty)

        if (leftOver.nonEmpty) throw new IllegalArgumentException(
          "Documents left over: " + leftOver.mkString(", ") + ".")

        val descriptor2documents: Map[Descriptor, Seq[Document]] =
          names2documents ++ fromAndCatchAll2documents

        descriptors.map { descriptor =>  new Part(
          Some(descriptor.getTitle),
          descriptor2documents.getOrElse(descriptor, throw new IllegalArgumentException("No documents for Part."))
        )}
      }
  }
}
