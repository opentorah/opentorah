/*
 * Copyright 2014-2018 Leonid Dubinsky <dub@podval.org>.
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


object HebrewLanguage extends Language {

  override def name: String = "he"


  val MAQAF       = "\u05BE"
  val PASEQ       = "\u05C0"
  val SOF_PASUQ   = "\u05C3"

  val ALEF        = "\u05D0"
  val BET         = "\u05D1"
  val GIMEL       = "\u05D2"
  val DALET       = "\u05D3"
  val HE          = "\u05D4"
  val VAV         = "\u05D5"
  val ZAYIN       = "\u05D6"
  val HET         = "\u05D7"
  val TET         = "\u05D8"
  val YOD         = "\u05D9"
  val KAF_SOFIT   = "\u05DA"
  val KAF         = "\u05DB"
  val LAMED       = "\u05DC"
  val MEM_SOFIT   = "\u05DD"
  val MEM         = "\u05DE"
  val NUN_SOFIT   = "\u05DF"
  val NUN         = "\u05E0"
  val SAMEH       = "\u05E1"
  val AYIN        = "\u05E2"
  val PEI_SOFIT   = "\u05E3"
  val PEI         = "\u05E4"
  val TSADI_SOFIT = "\u05E5"
  val TSADI       = "\u05E6"
  val QOF         = "\u05E7"
  val RESH        = "\u05E8"
  val SHIN        = "\u05E9"
  val TAV         = "\u05EA"


  private val units = Array(ALEF, BET, GIMEL, DALET, HE, VAV, ZAYIN, HET, TET)
  private val decades = Array(YOD, KAF, LAMED, MEM, NUN, SAMEH, AYIN, PEI, TSADI)
  private val hundreds = Array(QOF, RESH, SHIN, TAV)


  override def toString(number: Int): String = {
    require (number > 0)
    require (number <= 500)

    val result = new StringBuilder
    var remainder = number

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


  private val spelledOuts: Array[String] = Array(
    RESH + ALEF + SHIN + VAV + NUN_SOFIT,
    SHIN + NUN + YOD,
    SHIN + LAMED + YOD + SHIN + YOD,
    RESH + BET + YOD + AYIN + YOD,
    HET + MEM + YOD + SHIN + YOD,
    SHIN + SHIN + YOD,
    SHIN + BET + YOD + AYIN + YOD
  )


  override def numberToSpelledOutString(number: Int): String = spelledOuts(number - 1)
}
