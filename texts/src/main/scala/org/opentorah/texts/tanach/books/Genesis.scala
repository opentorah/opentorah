package org.opentorah.texts.tanach.books

import scala.xml.Elem

object Genesis {
  val chassanBereishis: Elem =
    <torah book="Genesis" fromChapter="1" fromVerse="1" toChapter="2" toVerse="3">
      <aliyah n="1" fromVerse="1"/>
    </torah>

  val roshHashana1torah: Elem =
    <torah book="Genesis" fromChapter="21" fromVerse="1" toVerse="34">
      <aliyah n="2" fromVerse="5"/>
      <aliyah n="3" fromVerse="9"/>
      <aliyah n="4" fromVerse="13"/>
      <aliyah n="5" fromVerse="18"/>
      <aliyah n="6" fromVerse="22"/>
      <aliyah n="7" fromVerse="28"/>
    </torah>

  val roshHashanah2torah: Elem =
    <torah book="Genesis" fromChapter="22" fromVerse="1" toVerse="24">
      <aliyah n="2" fromVerse="4"/>
      <aliyah n="3" fromVerse="9"/>
      <aliyah n="4" fromVerse="15"/>
      <aliyah n="5" fromVerse="20"/>
    </torah>
}
