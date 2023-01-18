package org.opentorah.astronomy

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

  // Ibn Habib on Pirush gives Albatani value as:
  final val albataniValue: Rotation = Rotation("0°59′8″20‴35")

  // Moznaim Rambam in English gives this value in KH 12:1 note 1 (without a reference) as the one
  // Rambam uses in his calculations.
  final override val rambamValue: Rotation = Rotation("0°59′8″19‴48")

  final override val almagestValue: Rotation = Rotation("0°59′8″17‴13,12,31")
