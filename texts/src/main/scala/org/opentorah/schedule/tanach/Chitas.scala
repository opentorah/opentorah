package org.opentorah.schedule.tanach

import org.opentorah.calendar.jewish.Jewish.{Day, Year}
import org.opentorah.calendar.jewish.SpecialDay
import org.opentorah.texts.tanach.{Custom, Parsha, Torah, WeeklyReading}
import org.opentorah.texts.tanach.Parsha.{Bereishis, VezosHaberachah}
import org.opentorah.util.Cache

// only on SimchasTorah is there a second fragment
final class Chitas private(val first: Torah.Fragment, val second: Option[Torah.Fragment])

object Chitas:
  def apply(day: Day, weeklyReading: WeeklyReading, inHolyLand: Boolean): Chitas =
    val numberInWeek: Int = day.numberInWeek

    val isBereishis = weeklyReading.parsha == Bereishis
    val simchasTorah = simchasTorahs.get((day.year, inHolyLand))
    if !isBereishis || day > simchasTorah then new Chitas(
      first = weeklyReading.getMorningReading.torah.doFind(Custom.Chabad).spans(numberInWeek-1),
      second = None
    ) else if day < simchasTorah then new Chitas(
      first = chabadFragments(VezosHaberachah)(numberInWeek-1),
      second = None
    ) else
      def forFragments(parsha: Parsha, selector: Seq[Torah.Fragment] => Seq[Torah.BookSpan]): Torah.Fragment =
        Torah.merge(selector(chabadFragments(parsha)))
      new Chitas(
        first = forFragments(VezosHaberachah, _.drop(numberInWeek - 1)),
        second = Some(forFragments(Bereishis, _.take(numberInWeek)))
      )

  private def chabadFragments(parsha: Parsha): Seq[Torah.Fragment] =
    parsha.days.doFind(Custom.Chabad).fromWithNumbers(parsha).spans

  private val simchasTorahs = new Cache[(Year, Boolean), Day]:
    override def calculate(args: (Year, Boolean)): Day = args match
      case (year: Year, inHolyLand: Boolean) =>
        val specialDay: SpecialDay =
          if inHolyLand then SpecialDay.SheminiAtzeresAndSimchasTorahInHolyLand else SpecialDay.SimchasTorah
        specialDay.date(year)
