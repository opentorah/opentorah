package org.podval.calendar.tanach

import org.podval.calendar.jewish.Jewish.{Day, Year}
import org.podval.judaica.metadata.Names
import org.podval.judaica.tanach.{Custom, Parsha, Torah}
import org.podval.judaica.tanach.Parsha.{Bereishis, VezosHaberachah}
import org.podval.judaica.util.Cache

// only on SimchasTorah is there a second fragment
final case class Chitas private(first: Chitas.Fragment, second: Option[Chitas.Fragment])

object Chitas {
  final case class Fragment(names: Names, torah: Torah.Fragment)

  def apply(day: Day, currentWeeklyReading: WeeklyReading, inHolyLand: Boolean): Chitas = {
    val numberInWeek: Int = day.numberInWeek

    def forCustoms(names: Names, customs: Torah.Customs): Chitas = Chitas(
      first = Fragment(names, chabadTorah(customs)(numberInWeek-1)),
      second = None
    )

    def forFragments(parsha: Parsha, selector: Seq[Torah.Fragment] => Seq[Torah.BookSpan]): Fragment =
      Fragment(parsha.names, Torah.merge(selector(chabadTorah(parsha.days))))

    def forReading: Chitas = {
      val reading = currentWeeklyReading.getMorningReading
      forCustoms(reading.names.get, reading.torah)
    }

    val isBereishis = currentWeeklyReading.parsha == Bereishis
    if (!isBereishis) forReading else {
      val simchasTorah = simchasTorahs.get((day.year, inHolyLand))
      if (day > simchasTorah) forReading else
      if (day == simchasTorah) Chitas(
        first = forFragments(VezosHaberachah, _.drop(numberInWeek - 1)),
        second = Some(forFragments(Bereishis, _.take(numberInWeek)))
      ) else
        forCustoms(VezosHaberachah.names, VezosHaberachah.days)
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
