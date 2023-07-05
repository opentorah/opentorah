package org.opentorah.astronomy

import Angles.Rotation
import Days2Rotation.Days

object MoonLongitudeMean extends Days2Rotation("mlm",
  // KH 14:1
  Days.One         -> " 13°10′35″",
  // KH 14:2
  Days.Ten         -> "131°45′50″",
  Days.Hundred     -> "237°38′23″",
  Days.Thousand    -> "216°23′50″",
  Days.TenThousand -> "  3°58′20″",
  Days.Month       -> " 22° 6′56″",
  Days.Year        -> "344°26′43″"
):
  val almagestValue = Rotation("13°10′34″58‴33′‴30″‴30‴‴")

  val rambamValue = Rotation("13°10′35″1‴48′‴1″‴") // TODO ?
