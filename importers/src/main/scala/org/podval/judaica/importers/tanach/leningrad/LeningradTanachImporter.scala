package org.podval.judaica.importers.tanach.leningrad

import java.io.File

import org.digitaljudaica.store.metadata.Xml
import org.digitaljudaica.store.metadata.Xml.Ops
import org.podval.judaica.importers.tanach.TanachImporter
import org.podval.judaica.viewer.{DivContent, HebrewLanguage, SpanContent, TextContent}
import org.podval.judaica.parsers.DivContentParser

import scala.xml.{Elem, Node}

final class LeningradTanachImporter extends TanachImporter {

  protected override def editionName: String = "Leningrad"

  protected override def books: Map[String, String] = Map(
    "Genesis" -> "Genesis"
  )

  protected override def getInputExtension: String = "xml"

  protected def parseBook(file: File, bookName: String): DivContent = {
    val chapters = Xml.load(file, "Tanach").elemsFilter("tanach").head.elemsFilter("book").head.elemsFilter("c")

    DivContent(
      sort = "book",
      n = Some(bookName),
      attributes = Node.NoAttributes,
      head = None,
      children = chapters.map { chapter =>
        DivContent(
          sort = "chapter",
          n = Some(chapter.intAttribute("n").toString),
          attributes = Node.NoAttributes,
          head = None,
          children = chapter.elemsFilter("v").map { verse =>
            DivContent(
              sort = "verse",
              n = Some(chapter.intAttribute("n").toString),
              attributes = Node.NoAttributes,
              head = None,
              children = verse.elemsFilter("w").map(parseWord) :+ SpanContent("sofpasuk", HebrewLanguage.SOF_PASUQ)
            )
          }
        )
      }
    )
  }

  private def parseWord(elem: Elem): DivContent = {
    val text = elem.text
    val hasMaqaf: Boolean = text.contains(HebrewLanguage.MAQAF)
//    val isPasek = line.consume("|")
    val result = text
      .replace("/", "")
      .replace("" + HebrewLanguage.SOF_PASUQ, "")
      .replace("" + HebrewLanguage.MAQAF, "")

    DivContent(
      sort = "word",
      n = None,
      attributes = DivContentParser.prependAttribute("makaf", hasMaqaf, Node.NoAttributes),
      head = None,
      children = Seq(TextContent(result))
    )
  }
}


object LeningradTanachImporter {
  def main(args: Array[String]): Unit = {
    new LeningradTanachImporter().importWork("/home/dub/Projects/judaica/data/imports/Tanach/leningrad")
  }
}
