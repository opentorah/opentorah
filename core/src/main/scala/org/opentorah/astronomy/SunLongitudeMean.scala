package org.opentorah.astronomy

import org.opentorah.numbers.BigRational
import Angles.Rotation
import Days2Rotation.Days

object SunLongitudeMean extends Days2Rotation("slm",
  // KH 12:1
  Days.One         -> "  0°59′ 8″",
  Days.Ten         -> "  9°51′23″",
  Days.Hundred     -> " 98°33′53″",
  Days.Thousand    -> "265°38′50″", // remainder
  Days.TenThousand -> "136°28′20″", // remainder
  Days.Month       -> " 28°35′ 1″",
  Days.Year        -> "348°55′15″"
):
  def fromSolarYearLength(solarYearLength: BigRational): Rotation = Angles.period/(solarYearLength, 6)
  
  val almagestValue: Rotation = Rotation("0°59′8″17‴13′‴12″‴31‴‴")

  // Pirush; Wikipedia: 365 days, 5 hours, 55 minutes and 12 seconds
  val almagestSolarYearLength: BigRational = BigRational(365) + BigRational(1, 4) - BigRational(1, 300)
  
  // Ibn Habib on Pirush gives al-Battani value as:
  final val alBattaniPirushValue: Rotation = Rotation("0°59′8″20‴35′‴")

  // Rambam Mevuar Inyanim gives the length of the year for al-Battani according to Pirush as 365d5h816p -?
  // Wikipedia gives 365 days, 5 hours, 46 minutes and 24 seconds.
  val alBattaniPirushSolarYearLength: BigRational = BigRational(365) + BigRational(1, 4) - BigRational(3, 360) - BigRational(40, 360 * 60)

  val alBattaniNeugebauerValue: Rotation = Rotation("0°59′8″20‴46′‴56″‴14‴‴")

  // LaAm, Tzikuni
  val rambamValue: Rotation = Rotation("0°59′8″19‴48′‴")
