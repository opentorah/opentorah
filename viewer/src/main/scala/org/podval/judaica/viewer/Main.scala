/*
 *  Copyright 2013 Leonid Dubinsky <dub@podval.org>.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.podval.judaica.viewer

import org.podval.judaica.xml.{Div, Word, AlefBeth, Xml, Load, App, Paragraph}

import org.podval.judaica.html
import html.{Span, Html}

import java.io.File

import scala.xml.Elem


/*
    This is for experiments with transforming the XML files for display.
    When properly generalized to be driven by the metadata, this will go away.
 */

object Main {

  def main(args: Array[String]) {
    val xml = Load.loadFile(new File("/home/dub/Code/judaica/tmp/Genesis.xml"))
    val output = new File("/home/dub/Code/judaica/tmp/Genesis.html")
//    val result = transformBook(xml)
    val result = TanachStructure.book.displayDiv(xml)
    Xml.print(Html("tanach", result), output)
  }

  // TODO make transform metadata-driven: divs allowed within a div; empty div -> name; name-conversion functions...


  private def transformBook(elem: Elem): Elem = elem match {
    case Div("book", n, children) => div("book", n, children.map(transformChapter))
  }


  private def transformChapter(elem: Elem): Elem = elem match {
    case Div("chapter", n, children) => div("chapter", HebrewNumbers.fromInt(n.toInt), children.map(transformChapterElement))
  }


  private def transformChapterElement(elem: Elem): Elem = elem match {
    case Div("verse", n, children) => div("verse", HebrewNumbers.fromInt(n.toInt),
      children.flatMap(transformVerseElement) :+ Span(AlefBeth.SOF_PASUQ, "sofpasuk")
    )

    case Div("week", n, _) => html.Name("week", n)
    case Div("day", n, _) => html.Name("day", "[" + HebrewNumbers.ordinal(n.toInt) + "]")
    case Div("maftir", _, _) => html.Name("maftir", "[" + HebrewNumbers.maftir + "]")
    case e@Paragraph(_, _) => transformParagraph(e)
  }


  private def transformVerseElement(elem: Elem): Seq[Elem] = elem match {
    case e@Paragraph(_, _) => Seq(transformParagraph(e))
    case e@Word(_, _, _) => transformWord(e)
    case App(read, write) => transformWord(read) :+ Span("[" + write + "]", "write")
  }


  private def transformParagraph(elem: Elem): Elem = elem match {
    case Paragraph(open, big) => html.Name("paragraph", if (open) AlefBeth.PEI else AlefBeth.SAMEH)
  }


  def transformWord(elem: Elem): Seq[Elem] = elem match {
    case Word(text, hasMakaf, hasPasek) =>
      Seq(Span(text + (if (hasMakaf) AlefBeth.MAQAF else ""), "word")) ++
        (if (hasPasek) Seq(Span(AlefBeth.PASEQ, "pasek")) else Seq())
  }


  def div(type_ : String, name: String, contents: Seq[Elem]) = html.Div(type_, html.Name(type_, name) +: contents )
}
