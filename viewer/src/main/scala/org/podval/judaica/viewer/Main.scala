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

import org.podval.judaica.xml.{Div, Word, AlefBeth, Xml, Load}
import org.podval.judaica.html.{Span, Html}

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


  private def transform(elem: Elem): Elem = elem match {
    case e@Div(type_) =>
      val n = Xml.getAttribute(elem, "n").trim
      val children: Seq[Elem] = Xml.elems(elem)
      // TODO rework to DivHtml
      // TODO Avoid unneded divs
      // TODO copy ALL attributes?
      <div type={type_} n={n}>
        { Span(name(type_, n, elem), "name", type_) }
        { if (type_ == "verse") transformVerse(children) else children.map(transform(_)) }
      </div>
    case e => e
  }


  def name(type_ : String, n: String, xml: Elem): String = type_ match {
    case "chapter" => HebrewNumbers.fromInt(n.toInt)
    case "verse" => HebrewNumbers.fromInt(n.toInt)
    case "day" => "[" + HebrewNumbers.ordinal(n.toInt) + "]"
    case "paragraph" => if (Xml.getBooleanAttribute(xml, "open")) AlefBeth.PEI else AlefBeth.SAMEH
    case _ => n
  }


  def transformVerse(words: Seq[Elem]): Seq[Elem] =
    (words flatMap transformVerseElement) :+ Span(AlefBeth.SOF_PASUQ, "sofpasuk")


  def transformVerseElement(elem: Elem): Seq[Elem] = elem match {
    case e@Word(_, _, _) => transformWord(e)
    case e@Div("app") => transformApp(e)
    case e@Div(_) => Seq(transform(e))
    case e => Seq(e)
  }


  def transformApp(elem: Elem): Seq[Elem] = {
    // TODO add App.unapply and use it!
    val readings: Seq[Elem] = (elem \ "rdg").map(_.asInstanceOf[Elem])
    val read = readings.find(Xml.isDiv(_, "read")).get
    val write = readings.find(Xml.isDiv(_, "write")).get
    transformWord(Xml.oneChild(read, "div")) :+ Span("[" + Xml.oneChild(write, "div").text + "]")
  }


  def transformWord(elem: Elem): Seq[Elem] = elem match {
    case Word(text, hasMakaf,hasPasek) =>
      Seq(Span(text + (if (hasMakaf) AlefBeth.MAQAF else ""), "word")) ++
      (if (hasPasek) Seq(Span(AlefBeth.PASEQ, "pasek")) else Seq())
  }
}
