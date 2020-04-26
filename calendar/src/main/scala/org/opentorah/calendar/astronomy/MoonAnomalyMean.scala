package org.opentorah.calendar.astronomy

import org.opentorah.calendar.angles.Angles.Rotation

object MoonAnomalyMean extends Time2Rotation {
  // KH 14:3
  final override val one        : Rotation = Rotation( 13,  3, 54)
  final override val ten        : Rotation = Rotation(130, 39,  0)
  final override val hundred    : Rotation = Rotation(226, 29, 53)
  final override val thousand   : Rotation = Rotation(104, 58, 50)
  final override val tenThousand: Rotation = Rotation(329, 48, 20)

  final override val month      : Rotation = Rotation( 18, 53,  4)
  // KH 14:4
  final override val year       : Rotation = Rotation(305,  0, 13)

  final override val rambamValue: Rotation = Rotation(13, 3, 53, 55, 49)

  final override val almagestValue = Rotation(13, 3, 53, 56, 17, 51, 59)

// Exactification:
//    [13°3′53″29‴29,29,30..13°3′54″29‴29,29,29] (6)    13°3′54″ * 1 -> 13°3′54″: [13°3′54″..13°3′54″] (2)
//    [13°3′53″56‴56,56,57..13°3′54″ 2‴56,56,56] (6)    13°3′54″ * 10 -> 130°39′: [13°3′54″..13°3′54″] (2)
//    [13°3′53″55‴29,41,42..13°3′53″56‴ 5,41,41] (6)    13°3′54″ * 100 -> 226°29′53″: [13°3′53″56‴..13°3′53″56‴] (3)
//    [13°3′53″55‴46,10,11..13°3′53″55‴49,46,10] (6)    13°3′54″ * 1000 -> 104°58′50″: [13°3′53″55‴47..13°3′53″55‴49] (4)
//    [13°3′53″55‴47,49, 2..13°3′53″55‴48,10,37] (6)    13°3′54″ * 10000 -> 329°48′20″: [13°3′53″55‴48..13°3′53″55‴48] (4)
//    [13°3′53″54‴48,36,12..13°3′53″56‴52,44,27] (6)    13°3′54″ * 29 -> 18°53′4″: [13°3′53″55‴..13°3′53″56‴] (3)
//    [13°3′53″56‴ 0,55,51..13°3′53″56‴11,6    ] (6)    13°3′54″ * 354 -> 305°0′13″: [13°3′53″56‴1..13°3′53″56‴11] (4)
}
