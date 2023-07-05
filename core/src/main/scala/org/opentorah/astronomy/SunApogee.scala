package org.opentorah.astronomy

import Angles.{Digit, Rotation}
import Days2Rotation.Days

object SunApogee extends Days2Rotation("sa",
  // KH 12:2
  Days.One         -> "0°", // Rambam doesn't give this value
  Days.Ten         -> "0° 0′ 1″30‴",
  Days.Hundred     -> "0° 0′15″",
  Days.Thousand    -> "0° 2′30″",
  Days.TenThousand -> "0°25′   ",
  Days.Month       -> "0° 0′ 4″",
  Days.Year        -> "0° 0′53″"
):
  override protected def precision(days: Days): Angles.Digit =
    if days == Days.Ten then Digit.THIRDS else Digit.SECONDS
