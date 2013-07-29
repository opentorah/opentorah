/*
 *  Copyright 2011-2013 Leonid Dubinsky <dub@podval.org>.
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
 * under the License.
 */

package org.podval.judaica.structure

import org.podval.judaica.xml.{AlefBeth, Paragraph, Word, App}
import org.podval.judaica.html.Html

import scala.xml.Elem


object TanachStructure {

  val week = new DivElement("week")()


  val day = new DivElement("day", nameClassSuffix = Some("number"))() {
    override def displayName(n: String): Option[String] = {
      val num = n.toInt
      if (num == 1) None else Some("[" + HebrewNumbers.ordinal(num) + "]")
    }
  }


  val maftir = new DivElement("maftir")() {
    override def displayName(n: String): Option[String] = Some("[" + MAFTIR + "]")
  }


  val paragraph = new DivElement("paragraph", nameClassSuffix = None)() {
    override protected def prefix(elem: Elem): Seq[Elem] = elem match {
      case Paragraph(open, _) => nameSpan(Some(if (open) AlefBeth.PEI else AlefBeth.SAMEH)).toSeq
    }
  }


  val word = new DivElement("word")() {
    override def display(elem: Elem): Seq[Elem] = transformWord(elem)
  }


  val app = new NonDivElement("app") {
    override def display(elem: Elem): Seq[Elem] = elem match {
      case App(read, write) => transformWord(read) :+ Html.span("write", "[" + write + "]")
    }
  }


  private def transformWord(elem: Elem): Seq[Elem] = elem match {
    case Word(wordText, hasMakaf, hasPasek) =>
      val (class_, text) = if (hasMakaf) ("word, hasMakaf", wordText + AlefBeth.MAQAF) else ("word", wordText)
      Seq(Html.span(class_, text)) ++ (if (hasPasek) Seq(Html.span("pasek", AlefBeth.PASEQ)) else Seq.empty)
  }


  val verse = new DivElement("verse", nameClassSuffix = Some("number"))(paragraph, word, app) {
    override def displayName(n: String): Option[String] = Some(HebrewNumbers.fromInt(n.toInt))
    override def suffix(elem: Elem): Seq[Elem] = Seq(Html.span("sofpasuk", AlefBeth.SOF_PASUQ))
  }


  val chapter = new DivElement("chapter", nameClassSuffix = Some("number"))(verse, week, day, maftir, paragraph) {
    override def displayName(n: String): Option[String] = Some(HebrewNumbers.fromInt(n.toInt))
  }


  val book = new DivElement("book")(chapter)


  val MAFTIR: String = AlefBeth.MEM + AlefBeth.PEI + AlefBeth.TET + AlefBeth.YOD + AlefBeth.RESH
}
