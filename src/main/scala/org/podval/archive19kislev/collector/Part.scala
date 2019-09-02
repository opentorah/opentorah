package org.podval.archive19kislev.collector

import scala.xml.Elem
import Xml.Ops

final class Part(val documents: Seq[Document]) {
}

object Part {

  final class Descriptor(val documentNames: Seq[String]) {
    def isCatchAll: Boolean = documentNames.isEmpty
  }

  def toDescriptor(xml: Elem): Descriptor =
    new Descriptor(documentNames = xml.elemsFilter("document").map(_.text))

  final val catchAllDescriptor = new Descriptor(documentNames = Seq.empty)

  def assignDocuments(partDescriptors: Seq[Part.Descriptor], documents: Seq[Document]): Seq[Part] = {
    if (partDescriptors.count(_.isCatchAll) > 1) throw new IllegalArgumentException("Multiple catch-all parts!")
    val (prefix: Seq[Part.Descriptor], tail: Seq[Part.Descriptor]) = partDescriptors.span(!_.isCatchAll)
    val (catchAll: Option[Part.Descriptor], suffix: Seq[Part.Descriptor]) =
      if (tail.isEmpty) (None, Seq.empty) else (Some(tail.head), tail.tail)
    val (prefixParts: Seq[Part], documentsTail: Seq[Document]) = mkParts(prefix, documents)
    val (suffixParts: Seq[Part], catchAllDocuments: Seq[Document]) = mkParts(suffix, documentsTail)
    val catchAllPart: Option[Part] = if (catchAll.isEmpty) {
      if (catchAllDocuments.nonEmpty) throw new IllegalArgumentException(
        "Documents left over: " + catchAllDocuments.mkString(", ") + ".")
      None
    } else Some(new Part(catchAllDocuments))

    prefixParts ++ catchAllPart.toSeq ++ suffixParts
  }

  private def mkParts(partDescriptors: Seq[Part.Descriptor], documents: Seq[Document]):
  (Seq[Part], Seq[Document]) = mkParts(partDescriptors, documents, Seq.empty)

  @scala.annotation.tailrec
  private def mkParts(partDescriptors: Seq[Part.Descriptor], documents: Seq[Document], result: Seq[Part]):
  (Seq[Part], Seq[Document]) = partDescriptors match {
    case Nil => (result, documents)
    case d :: ds =>
      val partDocuments: Seq[Document] = d.documentNames.map { documentName: String =>
        documents.find(_.name == documentName).getOrElse {
          throw new IllegalArgumentException(s"Document $documentName missing - or already claimed.")
        }
      }
      mkParts(
        ds,
        documents.filterNot(document =>  d.documentNames.contains(document.name)),
        result :+ new Part(partDocuments)
      )
  }
}
