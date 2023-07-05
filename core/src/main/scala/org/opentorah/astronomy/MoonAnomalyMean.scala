package org.opentorah.astronomy

import Angles.Rotation
import Days2Rotation.Days

object MoonAnomalyMean extends Days2Rotation("mam",
  // KH 14:3
  Days.One         -> " 13° 3′54″",
  Days.Ten         -> "130°39′ 0″",
  Days.Hundred     -> "226°29′53″",
  Days.Thousand    -> "104°58′50″",
  Days.TenThousand -> "329°48′20″",
  Days.Month       -> " 18°53′ 4″",
  // KH 14:4
  Days.Year        -> "305° 0′13″"
):
  val almagestValue = Rotation("13°3′53″56‴17′‴51″‴59‴‴")

  val rambamValue: Rotation = Rotation("13°3′53″55‴49′‴") // TODO ?
