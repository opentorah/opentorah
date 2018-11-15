package org.podval.calendar.schedule.tanach

import org.podval.judaica.metadata.tanach.BookSpan.ChumashSpan
import org.podval.judaica.metadata.tanach.{Custom, Parsha}

object Deuteronomy extends TorahReadings {
  val tishaBeAvTorah: Torah = parseTorah(
    <torah book="Deuteronomy" fromChapter="4" fromVerse="25" toVerse="40">
      <aliyah n="2" fromVerse="30"/>
      <aliyah n="3" fromVerse="36"/>
    </torah>)

  private val festivalEndShabbosTorahTorah: Torah = parseTorah(
    <torah book="Deuteronomy" fromChapter="14" fromVerse="22" toChapter="16" toVerse="17">
      <aliyah n="2" fromChapter="15" fromVerse="1"/>
      <aliyah n="3" fromChapter="15" fromVerse="19"/>
      <aliyah n="4" fromChapter="16" fromVerse="1"/>
      <aliyah n="5" fromChapter="16" fromVerse="4"/>
      <aliyah n="6" fromChapter="16" fromVerse="9"/>
      <aliyah n="7" fromChapter="16" fromVerse="13"/>
    </torah>)

  val sheminiAtzeresTorah: (Torah, Torah) = (festivalEndShabbosTorahTorah, drop(festivalEndShabbosTorahTorah, Set(2, 3)))

  val festivalEndTorah: (Torah, Torah) = (festivalEndShabbosTorahTorah, festivalEndShabbosTorahTorah.drop(2))

  val parshasZachorMaftir: ChumashSpan.BookSpan = parseMaftir(
      <maftir book="Deuteronomy" fromChapter="25" fromVerse="17" toVerse="19"/>)

  // TODO check that there isn't anything other than Common:
  val zosHaberachaIn6: Torah = TorahReadings.torah7to6(Parsha.VezosHaberachah.days(Custom.Common))
}
