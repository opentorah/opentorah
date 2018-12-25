package org.podval.calendar.tanach

import org.podval.judaica.metadata.WithNames
import org.podval.judaica.tanach.Torah
import org.podval.judaica.tanach.Torah.Maftir

object Numbers extends TorahReadings  {
  def chanukahFirst(source: WithNames): Torah = parseTorah(source,
    <torah book="Numbers" fromChapter="6" fromVerse="22" toChapter="7" toVerse="11">
      <aliyah n="2" fromChapter="7" fromVerse="1"/>
    </torah>
  )

  def chanukahKorbanotSpans(source: WithNames): Seq[Torah.Fragment] = parseTorah(source,
    <torah book="Numbers" fromChapter="7" fromVerse="12" toChapter="8" toVerse="4">
      <aliyah n="1"  fromVerse="12"/>
      <aliyah n="2"  fromVerse="15"/>
      <aliyah n="3"  fromVerse="18"/>
      <aliyah n="4"  fromVerse="21"/>
      <aliyah n="5"  fromVerse="24"/>
      <aliyah n="6"  fromVerse="27"/>
      <aliyah n="7"  fromVerse="30"/>
      <aliyah n="8"  fromVerse="33"/>
      <aliyah n="9"  fromVerse="36"/>
      <aliyah n="10" fromVerse="39"/>
      <aliyah n="11" fromVerse="42"/>
      <aliyah n="12" fromVerse="45"/>
      <aliyah n="13" fromVerse="48"/>
      <aliyah n="14" fromVerse="51"/>
      <aliyah n="15" fromVerse="54"/>
      <aliyah n="16" fromVerse="57"/>
      <aliyah n="17" fromVerse="60"/>
    </torah>).spans

  def pesach6torah(source: WithNames): Torah = parseTorah(source,
    <torah book="Numbers" fromChapter="9" fromVerse="1" toVerse="14">
      <aliyah n="2" fromVerse="7"/>
      <aliyah n="3" fromVerse="9"/>
    </torah>)

  val parshasParahMaftir: Maftir = parseMaftir(
      <maftir book="Numbers" fromChapter="19" fromVerse="1" toVerse="22"/>)

  def roshChodesh(source: WithNames): Torah = parseTorah(source,
    <torah book="Numbers" fromChapter="28" fromVerse="1" toVerse="15">
      <aliyah n="2" fromVerse="3"/>
      <aliyah n="3" fromVerse="4"/>
      <aliyah n="4" fromVerse="6"/>
      <aliyah n="5" fromVerse="9"/>
      <aliyah n="6" fromVerse="11"/>
    </torah>)

  val pesachMaftir: Maftir = parseMaftir(
      <maftir book="Numbers" fromChapter="28" fromVerse="16" toVerse="25"/>)

  // Maftir Pesach Intermediate Shabbos and last two days of Pesach
  val pesachEndMaftir: Maftir = parseMaftir(
      <maftir book="Numbers" fromChapter="28" fromVerse="19" toVerse="25"/>)

  val shavuosMaftir: Maftir = parseMaftir(
      <maftir book="Numbers" fromChapter="28" fromVerse="26" toVerse="31"/>)

  val roshHashanahMaftir: Maftir = parseMaftir(
      <maftir book="Numbers" fromChapter="29" fromVerse="1" toVerse="6"/>)

  val yomKippurMaftir: Maftir = parseMaftir(
      <maftir book="Numbers" fromChapter="29" fromVerse="7" toVerse="11"/>)

  def succosKorbanotSpans(source: WithNames): Seq[Torah.Aliyah] = parseTorah(source,
    <torah book="Numbers" fromChapter="29" fromVerse="12" toChapter="30" toVerse="1">
      <aliyah n="2" fromVerse="17"/>
      <aliyah n="3" fromVerse="20"/>
      <aliyah n="4" fromVerse="23"/>
      <aliyah n="5" fromVerse="26"/>
      <aliyah n="6" fromVerse="29"/>
      <aliyah n="7" fromVerse="32"/>
      <aliyah n="8" fromVerse="35"/>
    </torah>).spans
}
