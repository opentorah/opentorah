package org.podval.judaica.importers
package tanach

import org.podval.judaica.viewer.{HebrewLanguage, DivContent, Edition}


trait TanachImporter extends Importer {
  protected def workName: String = "Tanach"

  protected final override def processBook(content: DivContent, edition: Edition, outputName: String): DivContent = {
    val boundContent = DivContent.bind(content, edition.work, HebrewLanguage)


    // TODO Insert non-dominant structures

    boundContent
//    content
//    val breaks =
//      XmlFile.loadResource(classOf[TanachImporter], outputName, "meta").elems
//        .groupBy(_.getAttribute("chapter"))
//        .mapValues(_.groupBy(_.getAttribute("verse")).mapValues(_.map(dropChapterAndVerse)))
//
//    transformDiv(xml, "book") { book: Elem => flatMapChildren(book, {
//      transformDiv(_, "chapter") { chapter: Elem => flatMapChildren(chapter, {
//        transformDiv(_, "verse") { verse =>
//          val prefixBreaks: Seq[Elem] = breaks
//            .getOrElse(chapter.getAttribute("n"), Map.empty)
//            .getOrElse(verse.getAttribute("n"), Seq.empty)
//
//          val result: Seq[Elem] = prefixBreaks :+ verse
//          result
//        }
//      })}
//    })}(0).asInstanceOf[Elem]
  }


//  private def transformDiv(elem: Elem, divType: String)(f: Elem => Seq[Node]): Seq[Node] = {
//    val recognized = (elem.label == "div") && (elem.getAttribute("type") == divType)
//    if (recognized) f(elem) else Seq(elem)
//  }
//
//
//  private def flatMapChildren(elem: Elem, f: Elem => Seq[Node]): Elem = elem.copy(child = elem.elems flatMap f)
//
//
//  private def dropChapterAndVerse(break: Elem): Elem =
//    break.copy(attributes = break.attributes.filter(a => !TanachImporter.chapterAndVerse.contains(a.key)))
}


//object TanachImporter {
//
//  val chapterAndVerse = Set("chapter", "verse")
//}
