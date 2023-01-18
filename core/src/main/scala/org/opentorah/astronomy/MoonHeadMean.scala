package org.opentorah.astronomy

import Angles.Rotation
import Days2Rotation.Days

object MoonHeadMean extends Days2Rotation("mhm",
  // KH 16:2
  Days.One         -> "  0° 3′11″", // few thirds less (f6)
  Days.Ten         -> "  0°31′47″",
  Days.Hundred     -> "  5°17′43″",
  Days.Thousand    -> " 52°57′10″",
  Days.TenThousand -> "169°31′40″",
  Days.Month       -> "  1°32′ 9″",
  Days.Year        -> " 18°44′42″"
):

  final override val rambamValue = Rotation(0)

  final override val almagestValue = Rotation(0)
