package org.podval.calendar.tanach

import org.podval.calendar.jewish.Jewish.{Day, Year}
import org.podval.judaica.tanach.{Custom, Parsha, Torah}
import org.podval.judaica.tanach.Parsha.{Bereishis, VezosHaberachah}
import org.podval.judaica.util.Cache

// only on SimchasTorah is there a second fragment
final case class Chitas private(first: Torah.Fragment, second: Option[Torah.Fragment])

object Chitas {
  def apply(day: Day, currentWeeklyReading: WeeklyReading, inHolyLand: Boolean): Chitas = {
    val numberInWeek: Int = day.numberInWeek

    val isBereishis = currentWeeklyReading.parsha == Bereishis
    val simchasTorah = simchasTorahs.get((day.year, inHolyLand))
    if (!isBereishis || day > simchasTorah) Chitas(
      first = currentWeeklyReading.getMorningReading.torah.doFind(Custom.Chabad).spans(numberInWeek-1),
      second = None
    ) else if (day < simchasTorah) Chitas(
      first = chabadFragments(VezosHaberachah)(numberInWeek-1),
      second = None
    ) else {
      def forFragments(parsha: Parsha, selector: Seq[Torah.Fragment] => Seq[Torah.BookSpan]): Torah.Fragment =
        Torah.merge(selector(chabadFragments(parsha)))
      Chitas(
        first = forFragments(VezosHaberachah, _.drop(numberInWeek - 1)),
        second = Some(forFragments(Bereishis, _.take(numberInWeek)))
      )
    }
  }

  private def chabadFragments(parsha: Parsha): Seq[Torah.Fragment] =
    parsha.days.doFind(Custom.Chabad).fromWithNumbers(parsha).spans

  private val simchasTorahs = new Cache[(Year, Boolean), Day] {
    override def calculate(args: (Year, Boolean)): Day = args match {
      case (year: Year, inHolyLand: Boolean) =>
        val specialDay: SpecialDay.Date =
          if (inHolyLand) SpecialDay.SheminiAtzeresAndSimchasTorahInHolyLand else SpecialDay.SimchasTorah
        specialDay.date(year)
    }
  }
}
