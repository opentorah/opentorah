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

    def forCustoms(customs: Torah.Customs): Chitas = Chitas(
      first = chabadTorah(customs)(numberInWeek-1),
      second = None
    )

    def forFragments(parsha: Parsha, selector: Seq[Torah.Fragment] => Seq[Torah.BookSpan]): Torah.Fragment =
      Torah.merge(selector(chabadTorah(parsha.days)))

    def forReading: Chitas =
      forCustoms(currentWeeklyReading.getMorningReading.torah)

    val isBereishis = currentWeeklyReading.parsha == Bereishis
    if (!isBereishis) forReading else {
      val simchasTorah = simchasTorahs.get((day.year, inHolyLand))
      if (day > simchasTorah) forReading else
      if (day == simchasTorah) Chitas(
        first = forFragments(VezosHaberachah, _.drop(numberInWeek - 1)),
        second = Some(forFragments(Bereishis, _.take(numberInWeek)))
      ) else
        forCustoms(VezosHaberachah.days)
    }
  }

  private def chabadTorah(customs: Torah.Customs): Seq[Torah.BookSpan] =
    customs.doFind(Custom.Chabad).spans

  private val simchasTorahs = new Cache[(Year, Boolean), Day] {
    override protected def calculate(args: (Year, Boolean)): Day = args match {
      case (year: Year, inHolyLand: Boolean) =>
        val specialDay: SpecialDay.Date =
          if (inHolyLand) SpecialDay.SheminiAtzeresAndSimchasTorahInHolyLand else SpecialDay.SimchasTorah
        specialDay.date(year)
    }
  }
}
