package org.podval.calendar.astronomy

import angle.AngleNumberSystem.Angle


/*
 TODO
 Al-Battani, WikiPedia:
 One of Al-Battani's best-known achievements in astronomy was the refinement of existing values for the length of the year.
 Ptolemy calculated the solar year values for the length of the year as being 365 days, 5 hours, 55 minutes and 12 seconds.
 Al-Battani recalculated the solar year values for the length of the year as being 365 days, 5 hours, 46 minutes and 24 seconds.
 He was able to correct some of Ptolemy's results and compiled new tables of the Sun and Moon, long accepted as authoritative.
 Al-Battānī rediscovered that the direction of the Sun's apogee, as recorded by Ptolemy, was changing.
 (In modern heliocentric terms this is due to the changing direction eccentricity vector of the Earth's orbit).
 He also elaborated to a specified degree a number of trigonometric relations, the use of sines in calculation, and partially that of tangents.
 He elaborated to a specified degree the work of an Indian astronomer Aryabhata(476–550 CE) and a Greek astronomer Pythagoras (570 BC – c. 495 BC).
 He also recalculated the values for the precession of the equinoxes (54.5" per year, or 1° in 66 years) and the obliquity of the ecliptic (23° 35'),
 which was an elaboration of Hipparchus' work.
 */
trait DayData {

  type Days = Int

  val value: Map[Days, Angle]

  final def keys: List[Days] = value.keys.toList.sorted

  final def rambamValue = value(1)

  final def calculated(days: Days): Angle = rambamValue*days

  final def exact: Angle = reconstructed(10000)

  final def exact10: Double = reconstructed10(10000)

  final def reconstructed(days: Days): Angle =
    Angle.fromDegrees(reconstructed10(days), 6) // 6 60-digits

  final def reconstructed10(days: Days): Double = Angle.exactify(rambamValue, days, value(days))

  final def recalculated(days: Days): Angle = exact*days

  final def recalculated10(days: Days): Angle = Angle.fromDegrees(exact10*days, 6)

  val almagestValue: Angle

  final def almagest(days: Days): Angle = almagestValue*days
}
