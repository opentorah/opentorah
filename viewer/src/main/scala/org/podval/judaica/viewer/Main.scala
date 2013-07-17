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

import org.podval.judaica.xml.{Div, Word, AlefBeth, Xml, Load, App}

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
    val xml = Load.loadFile(new File("/tmp/xxx/Genesis.xml"))
    val output = new File("/tmp/xxx/Genesis.html")
    Xml.print(Html("tanach", transform(xml)), output)
  }


  private def transform(elem: Elem): Elem = {
    def div(type_ : String, name: String, contents: Seq[Elem]) = html.Div(type_, html.Name(name, type_) +: contents )

    elem match {
      case e@Div(type_, n, children) =>
        // TODO merge with "name()"!
        // TODO Avoid unneeded divs; what attributes for the remaining?
        div(type_,
          name(type_, n, elem),
          (if (type_ == "verse") transformVerse(children) else children.map(transform(_)))
        )
      case e => e
    }
  }


  def name(type_ : String, n: String, xml: Elem): String = type_ match {
    case "chapter" => HebrewNumbers.fromInt(n.toInt)
    case "verse" => HebrewNumbers.fromInt(n.toInt)
    case "day" => "[" + HebrewNumbers.ordinal(n.toInt) + "]"
    case "paragraph" => if (Xml.getBooleanAttribute(xml, "open")) AlefBeth.PEI else AlefBeth.SAMEH
    case "book" => n
    case "week" => n
    case "maftir" => n
    case n => n // TODO get rid of!
  }


  def transformVerse(words: Seq[Elem]): Seq[Elem] =
    (words flatMap transformVerseElement) :+ Span(AlefBeth.SOF_PASUQ, "sofpasuk")


  def transformVerseElement(elem: Elem): Seq[Elem] = elem match {
    case e@Word(_, _, _) => transformWord(e)
    case App(read, write) => transformWord(read) :+ Span("[" + write + "]")
    case e@Div(_, _, _) => Seq(transform(e))
    case e => Seq(e)
  }


  def transformWord(elem: Elem): Seq[Elem] = elem match {
    case Word(text, hasMakaf,hasPasek) =>
      Seq(Span(text + (if (hasMakaf) AlefBeth.MAQAF else ""), "word")) ++
      (if (hasPasek) Seq(Span(AlefBeth.PASEQ, "pasek")) else Seq())
  }
}
