package org.podval.calendar.schedule.tanach

import org.podval.judaica.metadata.tanach.BookSpan.ChumashSpan

object Deuteronomy extends TorahReadings {
  val tishaBeAvTorah: Torah = parseTorah(
    <torah book="Deuteronomy" fromChapter="4" fromVerse="25" toVerse="40">
      <aliyah n="2" fromVerse="30"/>
      <aliyah n="3" fromVerse="36"/>
    </torah>)

  val sheminiAtzeresTorah: (Torah, Torah) = parseTorahForShabbosAndWeekday(
    <torah book="Deuteronomy" fromChapter="14" fromVerse="22" toChapter="16" toVerse="17">
      <aliyah n="2" fromChapter="15" fromVerse="1" shabbos="true"/>
      <aliyah n="3" fromChapter="15" fromVerse="19" shabbos="true"/>
      <aliyah n="4" fromChapter="16" fromVerse="1"/>
      <aliyah n="5" fromChapter="16" fromVerse="4"/>
      <aliyah n="6" fromChapter="16" fromVerse="9"/>
      <aliyah n="7" fromChapter="16" fromVerse="13"/>
    </torah>)

  val parshasZachorMaftir: ChumashSpan.BookSpan = parseMaftir(
      <maftir book="Deuteronomy" fromChapter="25" fromVerse="17" toVerse="19"/>)

  // TODO 7th aliyah is swallowed by the 6th, BUT ALSO 2nd one starts at posuk 5 instead of 8?!
  val zosHaberachaIn6: Torah = parseTorah(
    <torah book="Deuteronomy" fromChapter="33" fromVerse="1" toChapter="34" toVerse="12">
      <aliyah n="2" fromChapter="33" fromVerse="5"/>
      <aliyah n="3" fromChapter="33" fromVerse="13"/>
      <aliyah n="4" fromChapter="33" fromVerse="18"/>
      <aliyah n="5" fromChapter="33" fromVerse="22"/>
      <aliyah n="6" fromChapter="33" fromVerse="27"/>
    </torah>)
}
