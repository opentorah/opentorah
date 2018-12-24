package org.podval.calendar.tanach

import org.podval.judaica.tanach.Torah
import org.podval.judaica.tanach.Torah.Maftir

object Deuteronomy extends TorahReadings {
  val tishaBeAvTorah: Torah = parseTorah(
    <torah book="Deuteronomy" fromChapter="4" fromVerse="25" toVerse="40">
      <aliyah n="2" fromVerse="30"/>
      <aliyah n="3" fromVerse="36"/>
    </torah>)

  private val (festivalEndShabbosTorah: Torah, sheminiAtzeresTorahWeekdayTorah: Torah) =
    parseTorahForShabbosAndWeekday(2, 3,
      <torah book="Deuteronomy" fromChapter="14" fromVerse="22" toChapter="16" toVerse="17">
        <aliyah n="2" fromChapter="15" fromVerse="1"/>
        <aliyah n="3" fromChapter="15" fromVerse="19"/>
        <aliyah n="4" fromChapter="16" fromVerse="1"/>
        <aliyah n="5" fromChapter="16" fromVerse="4"/>
        <aliyah n="6" fromChapter="16" fromVerse="9"/>
        <aliyah n="7" fromChapter="16" fromVerse="13"/>
      </torah>)

  val sheminiAtzeresTorah: (Torah, Torah) = (festivalEndShabbosTorah, sheminiAtzeresTorahWeekdayTorah)

  val festivalEndTorah: (Torah, Torah) = (festivalEndShabbosTorah, Torah(festivalEndShabbosTorah.spans.drop(2)))

  val parshasZachorMaftir: Maftir = parseMaftir(
      <maftir book="Deuteronomy" fromChapter="25" fromVerse="17" toVerse="19"/>)
}
