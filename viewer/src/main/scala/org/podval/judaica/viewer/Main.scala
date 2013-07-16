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

import scala.xml.{Node, Elem, Text}


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


  private def transform(xml: Node): Node = {
    if (Xml.isDiv(xml)) {
      val type_ = Xml.getType(xml)
      val n = Xml.getAttribute(xml, "n").trim
      val children: Seq[Elem] = xml.child.filter(_.isInstanceOf[Elem]).map(_.asInstanceOf[Elem])
      // TODO copy ALL attributes!!!

      <div type={type_} n={n}>
        <span class="name" type={type_}>{name(type_, n, xml)}</span>
        { if (type_ == "verse") transformVerse(children) else children.map(transform(_)) }
      </div>
    } else {
      xml
    }
  }


  def name(type_ : String, n: String, xml: Node): String = {
    if ((type_ == "chapter") || (type_ == "verse")) HebrewNumber.fromInt(n.toInt) else
    if (type_ == "day") "[" + HebrewNumber.ordinal(n.toInt) + "]" else
    if (type_ == "paragraph") { if (Xml.getBooleanAttribute(xml, "open")) AlefBeth.PEI else AlefBeth.SAMEH } else
      n
  }


  def transformVerse(words: Seq[Elem]): Seq[Node] = (words flatMap transformWord) ++ Text(AlefBeth.SOF_PASUQ)


  def transformWord(word: Elem): Seq[Node] = {
    word
  }
}


object HebrewNumber {

  import AlefBeth._
  val units = Array(ALEF, BET, GIMEL, DALET, HE, VAV, ZAYIN, HET, TET)
  val decades = Array(YOD, KAF, LAMED, MEM, NUN, SAMEH, AYIN, PEI, TSADI)
  val hundreds = Array(QOF, RESH, SHIN, TAV)


  def fromInt(n: Int): String = {
    require (n > 0)
    require (n <= 500)

    val result = new StringBuilder
    var remainder = n

    if (remainder >= 100) {
      result.append(hundreds((remainder / 100) - 1))
      remainder = remainder % 100
    }

    if (remainder == 15) {
      result.append(TET)
      result.append(VAV)
    } else

    if (remainder == 16) {
      result.append(TET)
      result.append(ZAYIN)
    } else {

      if (remainder >= 10) {
        result.append(decades((remainder / 10) - 1))
        remainder = remainder % 10
      }

      if (remainder >= 1) {
        result.append(units(remainder - 1))
      }
    }

    result.toString
  }


  import AlefBeth._

  val ordinals: Array[String] = Array(
    RESH + ALEF + SHIN + VAV + NUN_SOFIT,
    SHIN + NUN + YOD,
    SHIN + LAMED + YOD + SHIN + YOD,
    RESH + BET + YOD + AYIN + YOD,
    HET + MEM + YOD + SHIN + YOD,
    SHIN + SHIN + YOD,
    SHIN + BET + YOD + AYIN + YOD
  )


  def ordinal(n: Int): String = ordinals(n - 1)
}
