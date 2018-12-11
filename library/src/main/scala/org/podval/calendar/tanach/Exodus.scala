package org.podval.calendar.tanach

import org.podval.judaica.tanach.Torah
import org.podval.judaica.tanach.Torah.Maftir

object Exodus extends TorahReadings {
  val parshasHachodeshMaftir: Maftir = parseMaftir(
      <maftir book="Exodus" fromChapter="12" fromVerse="1" toVerse="20"/>)

  val pesach1torah: (Torah, Torah) = parseTorahForShabbosAndWeekday(4, 7,
    <torah book="Exodus" fromChapter="12" fromVerse="21" toVerse="51">
      <aliyah n="2" fromVerse="25"/>
      <aliyah n="3" fromVerse="29"/>
      <aliyah n="4" fromVerse="33"/>
      <aliyah n="5" fromVerse="37"/>
      <aliyah n="6" fromVerse="43"/>
      <aliyah n="7" fromVerse="48"/>
    </torah>)

  val pesach3torah: Torah = parseTorah(
    <torah book="Exodus" fromChapter="13" fromVerse="1" toVerse="16">
      <aliyah n="2" fromVerse="5"/>
      <aliyah n="3" fromVerse="11"/>
    </torah>)

  val pesach7torah: (Torah, Torah) = parseTorahForShabbosAndWeekday(2, 4,
    <torah book="Exodus" fromChapter="13" fromVerse="17" toChapter="15" toVerse="26">
      <aliyah n="2" fromChapter="13" fromVerse="20"/>
      <aliyah n="3" fromChapter="14" fromVerse="1"/>
      <aliyah n="4" fromChapter="14" fromVerse="5"/>
      <aliyah n="5" fromChapter="14" fromVerse="9"/>
      <aliyah n="6" fromChapter="14" fromVerse="15"/>
      <aliyah n="7" fromChapter="14" fromVerse="26"/>
    </torah>)

  val purimTorah: Torah = parseTorah(
    <torah book="Exodus" fromChapter="17" fromVerse="8" toVerse="16">
      <aliyah n="2" fromVerse="11"/>
      <aliyah n="3" fromVerse="14"/>
    </torah>)

  val shavuosTorah: Torah = parseTorah(
    <torah book="Exodus" fromChapter="19" fromVerse="1" toChapter="20" toVerse="22">
      <aliyah n="2" fromChapter="19" fromVerse="7"/>
      <aliyah n="3" fromChapter="19" fromVerse="14"/>
      <aliyah n="4" fromChapter="19" fromVerse="20"/>
      <aliyah n="5" fromChapter="20" fromVerse="15"/>
    </torah>)

  val pesach4torah: Torah = parseTorah(
    <torah book="Exodus" fromChapter="22" fromVerse="24" toChapter="23" toVerse="19">
      <aliyah n="2" fromChapter="22" fromVerse="27"/>
      <aliyah n="3" fromChapter="23" fromVerse="6"/>
    </torah>)

  val parshasShekalimMaftir: Maftir = parseMaftir(
      <maftir book="Exodus" fromChapter="30" fromVerse="11" toVerse="16"/>)

  private final val fastAfternoonTorahPart1: Torah = parseTorah(
    <torah book="Exodus" fromChapter="32" fromVerse="11" toVerse="14">
      <aliyah n="1" fromVerse="11"/>
    </torah>)

  val intermediateShabbosTorah: Torah = parseTorah(
    <torah book="Exodus" fromChapter="33" fromVerse="12" toChapter="34" toVerse="26">
      <aliyah n="2" fromChapter="33" fromVerse="17"/>
      <aliyah n="3" fromChapter="33" fromVerse="20"/>
      <aliyah n="4" fromChapter="34" fromVerse="1"/>
      <aliyah n="5" fromChapter="34" fromVerse="4"/>
      <aliyah n="6" fromChapter="34" fromVerse="11"/>
      <aliyah n="7" fromChapter="34" fromVerse="18"/>
    </torah>)

  private val fastAfternoonTorahPart2: Torah = {
    val all = intermediateShabbosTorah.spans
    Torah.aliyot(
      all(3), // 34:1-3
      all(4)  // 34:4-10
    )
  }

  val pesach5torah: Torah = {
    val all = intermediateShabbosTorah.spans
    Torah.aliyot(
      all(3),         // 34:1-3
      all(4)+all(5),  // 34:4-17
      all(6)          // 34:18-26
    )
  }

  val fastTorah: Torah = fastAfternoonTorahPart1 ++ fastAfternoonTorahPart2
}
