package org.podval.calendar.schedule.tanach

import org.podval.judaica.metadata.tanach.BookSpan.ChumashSpan

object Numbers extends TorahReadings  {
  // last aliyah is for Zos Channukah
  val channukahKorbanot: Torah = parseTorah(
    <torah book="Numbers" fromChapter="7" fromVerse="1" toChapter="8" toVerse="4">
      <aliyah n="1"  fromVerse="1" />
      <aliyah n="2"  fromVerse="12"/>
      <aliyah n="3"  fromVerse="15"/>
      <aliyah n="4"  fromVerse="18"/>
      <aliyah n="5"  fromVerse="21"/>
      <aliyah n="6"  fromVerse="24"/>
      <aliyah n="7"  fromVerse="27"/>
      <aliyah n="8"  fromVerse="30"/>
      <aliyah n="9"  fromVerse="33"/>
      <aliyah n="10" fromVerse="36"/>
      <aliyah n="11" fromVerse="39"/>
      <aliyah n="12" fromVerse="42"/>
      <aliyah n="13" fromVerse="45"/>
      <aliyah n="14" fromVerse="48"/>
      <aliyah n="15" fromVerse="51"/>
      <aliyah n="16" fromVerse="54"/>
      <aliyah n="17" fromVerse="57"/>
      <aliyah n="18" fromVerse="60"/>
    </torah>)

  val pesach6torah: Torah = parseTorah(
    <torah book="Numbers" fromChapter="9" fromVerse="1" toVerse="14">
      <aliyah n="2" fromVerse="7"/>
      <aliyah n="3" fromVerse="9"/>
    </torah>)

  val parshasParahMaftir: ChumashSpan.BookSpan = parseMaftir(
      <maftir book="Numbers" fromChapter="19" fromVerse="1" toVerse="22"/>)

  val roshChodesh: Torah = parseTorah(
    <torah book="Numbers" fromChapter="28" fromVerse="1" toVerse="15">
      <aliyah n="2" fromVerse="3"/>
      <aliyah n="3" fromVerse="4"/>
      <aliyah n="4" fromVerse="6"/>
      <aliyah n="5" fromVerse="9"/>
      <aliyah n="6" fromVerse="11"/>
    </torah>)

  val pesachMaftir: ChumashSpan.BookSpan = parseMaftir(
      <maftir book="Numbers" fromChapter="28" fromVerse="16" toVerse="25"/>)

  // TODO 4th aliyah Chol Hamoed, maftir Shabbos Chol Hamoed and last two days
  val pesachEndMaftir: ChumashSpan.BookSpan = parseMaftir(
      <maftir book="Numbers" fromChapter="28" fromVerse="19" toVerse="25"/>)

  val shavuosMaftir: ChumashSpan.BookSpan = parseMaftir(
      <maftir book="Numbers" fromChapter="28" fromVerse="26" toVerse="31"/>)

  val roshHashanahMaftir: ChumashSpan.BookSpan = parseMaftir(
      <maftir book="Numbers" fromChapter="29" fromVerse="1" toVerse="6"/>)

  val yomKippurMaftir: ChumashSpan.BookSpan = parseMaftir(
      <maftir book="Numbers" fromChapter="29" fromVerse="7" toVerse="11"/>)

  // TODO maftir for Shabbos: 1st day 17-22; 3rd 23-28; 4th 26-38
  val succosKorbanot: Torah = parseTorah(
    <torah book="Numbers" fromChapter="29" fromVerse="12" toVerse="34">
      <aliyah n="2" fromVerse="17"/>
      <aliyah n="3" fromVerse="20"/>
      <aliyah n="4" fromVerse="23"/>
      <aliyah n="5" fromVerse="26"/>
      <aliyah n="6" fromVerse="29"/>
      <aliyah n="7" fromVerse="32"/>
    </torah>)

  val sheminiAtzeresMaftir: ChumashSpan.BookSpan = parseMaftir(
      <maftir book="Numbers" fromChapter="29" fromVerse="35" toChapter="30" toVerse="1"/>)
}
