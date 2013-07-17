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

import org.podval.judaica.common.{AlefBeth, Xml}

import java.io.File

import scala.xml.{Elem, Text}

import scala.collection.mutable.ArrayBuffer


/*
    This is for experiments with transforming the XML files for display.
    When properly generalized to be driven by the metadata, this will go away.
 */

object Main {

  def main(args: Array[String]) {
    val xml = Xml.loadFile(new File("/tmp/xxx/Genesis.xml"))
    val output = new File("/tmp/xxx/Genesis.html")
    Xml.print(Xml.wrapInHtml("tanach", transform(xml)), output)
  }


  private def transform(elem: Elem): Elem = {
    if (Xml.isDiv(elem)) {
      val type_ = Xml.getType(elem)
      val n = Xml.getAttribute(elem, "n").trim
      val children: Seq[Elem] = Xml.elems(elem)
      // TODO copy ALL attributes!!!

      <div type={type_} n={n}>
        <span class="name" type={type_}>{name(type_, n, elem)}</span>
        { if (type_ == "verse") transformVerse(children) else children.map(transform(_)) }
      </div>
    } else {
      elem
    }
  }


  def name(type_ : String, n: String, xml: Elem): String = type_ match {
    case "chapter" => HebrewNumbers.fromInt(n.toInt)
    case "verse" => HebrewNumbers.fromInt(n.toInt)
    case "day" => "[" + HebrewNumbers.ordinal(n.toInt) + "]"
    case "paragraph" => if (Xml.getBooleanAttribute(xml, "open")) AlefBeth.PEI else AlefBeth.SAMEH
    case _ => n
  }


  def transformVerse(words: Seq[Elem]): Seq[Elem] =
    (words flatMap transformVerseElement) ++
    Seq(<span class="sofpasuk">{AlefBeth.SOF_PASUQ}</span>)


  def transformVerseElement(elem: Elem): Seq[Elem] = {
    if (Xml.isDiv(elem)) {
      Xml.getType(elem) match {
        case "word" => transformWord(elem)
        case "app" => transformApp(elem)
        case _ => Seq(transform(elem))
      }
    } else {
      Seq(elem)
    }
  }


  def transformApp(elem: Elem): Seq[Elem] = {
    val readings: Seq[Elem] = (elem \ "rdg").map(_.asInstanceOf[Elem])
    val read = readings.find(Xml.isDiv(_, "read")).get
    val write = readings.find(Xml.isDiv(_, "write")).get
    transformWord(Xml.oneChild(read, "div")) ++
    Seq(<span>[{Xml.oneChild(write, "div").text}]</span>)
  }


  def transformWord(elem: Elem): Seq[Elem] = {
    val hasMakaf = Xml.getBooleanAttribute(elem, "makaf")
    val hasPasek = Xml.getBooleanAttribute(elem, "pasek")
    val text = elem.text

    var result = new ArrayBuffer[Elem]
    result +=
      <span class="word">{text + (if (hasMakaf) AlefBeth.MAQAF else "")}</span>

    if (hasPasek) result +=
      <span class="pasek">{Text(AlefBeth.PASEQ)}</span>

    result
  }
}
