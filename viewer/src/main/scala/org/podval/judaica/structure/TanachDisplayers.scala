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

import org.podval.judaica.xml.{AlefBeth, Paragraph, Word, App, Html}

import scala.xml.Elem


object TanachDisplayers {

  val book = new DivElementDisplayer("book")


  val chapter = new DivElementDisplayer("chapter") {
    override def isNumeric: Boolean = true
  }


  val week = new DivElementDisplayer("week")


  val day = new DivElementDisplayer("day") {
    override def isNumeric: Boolean = true
    override def isOrdinal: Boolean = true
  }


  val verse = new DivElementDisplayer("verse") {
    override def isNumeric: Boolean = true
    override def suffix(elem: Elem): Option[Elem] = Some(Html.span("sofpasuk", AlefBeth.SOF_PASUQ))
  }


  val word = new DivElementDisplayer("word") {
    override def displayContent(elem: Elem): Seq[Elem] = displayWord(elem)
    override def suffix(elem: Elem): Option[Elem] = elem match {
      case Word(wordText, hasMakaf, hasPasek) => if (hasPasek) Some(Html.span("pasek", AlefBeth.PASEQ)) else None
    }
  }


  val app = new NonDivElementDisplayer("app") {
    override def display(elem: Elem, displayers: Set[ElementDisplayer]): Seq[Elem] = elem match {
      case App(read, write) => displayWord(read) :+ Html.span("write", "[" + write + "]")
    }
  }


  val paragraph = new DivElementDisplayer("paragraph") {
    override def suffix(elem: Elem): Option[Elem] = elem match {
      case Paragraph(open, _) => Some(Html.span("paragraph", if (open) AlefBeth.PEI else AlefBeth.SAMEH))
    }
  }


  val maftir = new DivElementDisplayer("maftir") {
    // TODO I really shouldn't be overriding displayContent: what if "maftir" is a populated div in the current structure?!
    override def displayContent(elem: Elem): Seq[Elem] = Seq(Html.span("maftir", MAFTIR))

  }


  val MAFTIR: String = AlefBeth.MEM + AlefBeth.PEI + AlefBeth.TET + AlefBeth.YOD + AlefBeth.RESH


  private def displayWord(elem: Elem): Seq[Elem] = elem match {
    case Word(wordText, hasMakaf, hasPasek) =>
      val (class_, text) = if (hasMakaf) ("word, hasMakaf", wordText + AlefBeth.MAQAF) else ("word", wordText)
      Seq(Html.span(class_, text))
  }


  val displayers: Set[ElementDisplayer] = Set(book, chapter, week, day, maftir, verse, word, app, paragraph)
}
