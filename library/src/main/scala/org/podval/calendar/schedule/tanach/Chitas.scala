package org.podval.calendar.schedule.tanach

import org.podval.calendar.jewish.Jewish.Day
import org.podval.judaica.metadata.Names
import org.podval.judaica.metadata.tanach.{Custom, Parsha, Torah}
import org.podval.judaica.metadata.tanach.Parsha.{Bereishis, VezosHaberachah}

// only on SimchasTorah is there a second fragment
final case class Chitas private(first: Chitas.Fragment, second: Option[Chitas.Fragment])

object Chitas {
  final case class Fragment(names: Names, torah: Torah.Fragment)

  def apply(day: Day, currentWeeklyReading: WeeklyReading): Chitas = {
    val numberInWeek: Int = day.numberInWeek

    def forParsha(parsha: Parsha): Chitas = forCustoms(parsha.names, parsha.days)

    def forCustoms(names: Names, customs: Torah.Customs): Chitas = Chitas(
      first = Fragment(names, chabadTorah(customs)(numberInWeek-1)),
      second = None
    )

    def forFragments(parsha: Parsha, selector: Seq[Torah.Fragment] => Seq[Torah.BookSpan]): Fragment =
      Fragment(parsha.names, Torah.merge(selector(chabadTorah(parsha.days))))

    val simchasTorah: Day = SpecialDay.SimchasTorah.date(day.year)
    val isSimchasTorahThisWeek: Boolean = simchasTorah.shabbosAfter == day.shabbosAfter
    if (!isSimchasTorahThisWeek) {
      val reading = currentWeeklyReading.getMorningReading
      forCustoms(reading.names.get, reading.torah)
    } else {
      if (day < simchasTorah) forParsha(VezosHaberachah) else
      if (day > simchasTorah) forParsha(Bereishis) else Chitas(
        first = forFragments(VezosHaberachah, _.drop(numberInWeek - 1)),
        second = Some(forFragments(Bereishis, _.take(numberInWeek)))
      )
    }
  }

  private def chabadTorah(customs: Torah.Customs): Seq[Torah.BookSpan] =
    customs.doFind(Custom.Chabad).spans
}
