package org.opentorah.astronomy

import org.opentorah.calendar.jewish.{Jewish, Season, Sun}
import org.opentorah.numbers.BigRational
import Angles.{Position, Rotation}
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

  // TODO unify with Sun somehow?
  sealed class Opinion(val name: String, base: Opinion.Base):
    final def isBasedOnVelocity: Boolean = base match
      case Opinion.Base.Velocity(_) => true
      case _ => false

    final def yearLength: Jewish.TimeVector = Jewish.TimeVector.fromRational(yearLengthRational, Jewish.maxLength)

    final def yearLengthRational: BigRational = base match
      case Opinion.Base.YearLength(value) => value
      case Opinion.Base.Velocity(value) => Angles.period.toRational/value.toRational

    final def movementInOneDay: Rotation = base match
      case Opinion.Base.Velocity(value) => value
      case Opinion.Base.YearLength(value) => Angles.period/(value, 6)

    final def isReconstructable(days: Days): Boolean = SunLongitudeMean.isReconstructable(movementInOneDay, days)

  object Opinion:
    enum Base:
      case YearLength(value: BigRational)
      case Velocity(value: Rotation)

    object Almagest extends Opinion("Ptolemy", Base.YearLength(BigRational(365) + BigRational(1, 4) - BigRational(1, 300)))

    object AlBattani:
      object Neugebauer extends Opinion("al-Battani - Neugebauer", Base.Velocity(Rotation("0°59′8″20‴46′‴56″‴14‴‴")))

      object Pirush:
        object FromMovement extends Opinion("al-Battani - Pirush", Base.Velocity(Rotation("0°59′8″20‴35′‴")))

        // = 365ᵈ5ʰ816ᵖ; TODO Pirush mentions this in 6:4 also?
        // Wikipedia gives 365 days, 5 hours, 46 minutes and 24 seconds.
        object FromYearLength extends Opinion(
          "al-Battani - Pirush (year)",
          Base.YearLength(BigRational(365) + BigRational(1, 4) - Rotation("3°40′").toRational/Angles.period.toRational)
        )

    object Rambam extends Opinion("Rambam", Base.Velocity(Rotation("0°59′8″19‴48′‴")))
    object RavAda extends Opinion("Rav Ada", Base.YearLength(Sun.RavAda.yearLength.toRational))
    object Shmuel extends Opinion("Shmuel", Base.YearLength(Sun.Shmuel.yearLength.toRational))

  // sun was created in the first hour of the fourth day of creation, at equinox
  sealed class SunAtCreation(
    val day: Jewish.Day,
    val season: Season
  ):
    final def position: Position = season.sunEnters.start

    lazy val velocity: Rotation.Interval =
      val exact = ExactRotation(
        // days to Rambam's epoch
        days = Epoch.Text.day.number - day.number,
        precision = 2,
        // assuming time the sun was created is the same as the time of the epoch
        value = (Epoch.Text.sunLongitudeMean - position).canonical
      )
      val (interval, minLength) = exact.findInside(6).get
      require(minLength == 6)

      exact.expand(interval, minLength, 6)

  object SunAtCreation:
    object Nisan extends SunAtCreation(
      Jewish.Year(1).month(Jewish.Month.Nisan).firstDay.prev.prev, // TODO move into Epoch or something
      Season.TkufasNisan
    )

    object Tishrei extends SunAtCreation(
      Jewish.Year(2).firstDay.prev.prev, // TODO move into Epoch or something
      Season.TkufasTishrei
    )
