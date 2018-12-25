package org.podval.calendar.tanach

import org.podval.judaica.metadata.WithNames
import org.podval.judaica.tanach.Torah

object Genesis extends TorahReadings {
  def chassanBereishis(source: WithNames): Torah.Aliyah = parseTorah(source,
    <torah book="Genesis" fromChapter="1" fromVerse="1" toChapter="2" toVerse="3">
      <aliyah n="1" fromVerse="1"/>
    </torah>).spans.head

  def roshHashana1torah(source: WithNames): (Torah, Torah) = parseTorahForShabbosAndWeekday(source, 3, 5,
    <torah book="Genesis" fromChapter="21" fromVerse="1" toVerse="34">
      <aliyah n="2" fromVerse="5"/>
      <aliyah n="3" fromVerse="9"/>
      <aliyah n="4" fromVerse="13"/>
      <aliyah n="5" fromVerse="18"/>
      <aliyah n="6" fromVerse="22"/>
      <aliyah n="7" fromVerse="28"/>
    </torah>)

  def roshHashanah2torah(source: WithNames): Torah = parseTorah(source,
    <torah book="Genesis" fromChapter="22" fromVerse="1" toVerse="24">
      <aliyah n="2" fromVerse="4"/>
      <aliyah n="3" fromVerse="9"/>
      <aliyah n="4" fromVerse="15"/>
      <aliyah n="5" fromVerse="20"/>
    </torah>)
}
