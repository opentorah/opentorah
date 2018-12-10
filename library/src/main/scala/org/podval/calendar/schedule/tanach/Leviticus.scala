package org.podval.calendar.schedule.tanach

import org.podval.judaica.metadata.tanach.Torah

object Leviticus extends TorahReadings {
  val yomKippurTorah: (Torah, Torah) = parseTorahForShabbosAndWeekday(Set(2),
    <torah book="Leviticus" fromChapter="16" fromVerse="1" toVerse="34">
      <aliyah n="2" fromVerse="4"/>
      <aliyah n="3" fromVerse="7"/>
      <aliyah n="4" fromVerse="12"/>
      <aliyah n="5" fromVerse="18"/>
      <aliyah n="6" fromVerse="25"/>
      <aliyah n="7" fromVerse="31"/>
    </torah>)

  val yomKippurAfternoonTorah: Torah = parseTorah(
    <torah book="Leviticus" fromChapter="18" fromVerse="1" toVerse="30">
      <aliyah n="2" fromVerse="6"/>
      <aliyah n="3" fromVerse="22"/>
    </torah>)

  val succos1and2torah: (Torah, Torah) = parseTorahForShabbosAndWeekday(2, 4,
    <torah book="Leviticus" fromChapter="22" fromVerse="26" toChapter="23" toVerse="44">
      <aliyah n="2" fromChapter="23" fromVerse="1"/>
      <aliyah n="3" fromChapter="23" fromVerse="4"/>
      <aliyah n="4" fromChapter="23" fromVerse="9"/>
      <aliyah n="5" fromChapter="23" fromVerse="15"/>
      <aliyah n="6" fromChapter="23" fromVerse="23"/>
      <aliyah n="7" fromChapter="23" fromVerse="33"/>
    </torah>)
}
