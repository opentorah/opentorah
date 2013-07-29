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

package org.podval.judaica.viewer

import org.podval.judaica.xml.AlefBeth._

import scala.Array


object HebrewNumbers {

  private val units = Array(ALEF, BET, GIMEL, DALET, HE, VAV, ZAYIN, HET, TET)
  private val decades = Array(YOD, KAF, LAMED, MEM, NUN, SAMEH, AYIN, PEI, TSADI)
  private val hundreds = Array(QOF, RESH, SHIN, TAV)


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


  private val ordinals: Array[String] = Array(
    RESH + ALEF + SHIN + VAV + NUN_SOFIT,
    SHIN + NUN + YOD,
    SHIN + LAMED + YOD + SHIN + YOD,
    RESH + BET + YOD + AYIN + YOD,
    HET + MEM + YOD + SHIN + YOD,
    SHIN + SHIN + YOD,
    SHIN + BET + YOD + AYIN + YOD
  )


  def ordinal(n: Int): String = ordinals(n - 1)


  // TODO move out from here
  val maftir: String = MEM + PEI + TET + YOD + RESH
}
