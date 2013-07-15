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

import java.io.File
import org.podval.judaica.common.{AlefBeth, Xml}
import scala.xml.{NodeBuffer, Node, Elem}


/*
    This is for experiments with transforming the XML files for display.
    When properly generalized to be driven by the metadata, this will go away.
 */

object Main {

  def main(args: Array[String]) {
    val xml = Xml.loadFile(new File("/tmp/xxx/Genesis.xml"))
    val output = new File("/tmp/xxx/Genesis.html")
    Xml.print(Xml.wrapInHtml("tanach", addNames(xml)), output)
  }


  private def addNames(xml: Node): Node = {
    if (Xml.isDiv(xml)) {
      val type_ = Xml.getType(xml)
      val n = Xml.getAttribute(xml, "n").trim
      val num = isNumeric(type_)
      val name = if (num) HebrewNumber.fromInt(n.toInt) else n

      <div type={type_} n={n}>
        <span class={type_ + "-" + (if (num) "number" else "name")}>{name}</span>
        {xml.child.filter(_.isInstanceOf[Elem]).map(addNames(_))}
      </div>
    } else {
      xml
    }
  }


  def isNumeric(type_ : String): Boolean = (type_ == "chapter") || (type_ == "verse")
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
}
