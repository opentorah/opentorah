package org.podval.calendar.tanach

import scala.xml.Elem

object Exodus {
  val parshasHachodeshMaftir: Elem =
      <maftir book="Exodus" fromChapter="12" fromVerse="1" toVerse="20"/>

  val pesach1torah: Elem =
    <torah book="Exodus" fromChapter="12" fromVerse="21" toVerse="51">
      <aliyah n="2" fromVerse="25"/>
      <aliyah n="3" fromVerse="29"/>
      <aliyah n="4" fromVerse="33"/>
      <aliyah n="5" fromVerse="37"/>
      <aliyah n="6" fromVerse="43"/>
      <aliyah n="7" fromVerse="48"/>
    </torah>

  val pesach3torah: Elem =
    <torah book="Exodus" fromChapter="13" fromVerse="1" toVerse="16">
      <aliyah n="2" fromVerse="5"/>
      <aliyah n="3" fromVerse="11"/>
    </torah>

  val pesach7torah: Elem =
    <torah book="Exodus" fromChapter="13" fromVerse="17" toChapter="15" toVerse="26">
      <aliyah n="2" fromChapter="13" fromVerse="20"/>
      <aliyah n="3" fromChapter="14" fromVerse="1"/>
      <aliyah n="4" fromChapter="14" fromVerse="5"/>
      <aliyah n="5" fromChapter="14" fromVerse="9"/>
      <aliyah n="6" fromChapter="14" fromVerse="15"/>
      <aliyah n="7" fromChapter="14" fromVerse="26"/>
    </torah>

  val purimTorah: Elem =
    <torah book="Exodus" fromChapter="17" fromVerse="8" toVerse="16">
      <aliyah n="2" fromVerse="11"/>
      <aliyah n="3" fromVerse="14"/>
    </torah>

  val shavuosTorah: Elem =
    <torah book="Exodus" fromChapter="19" fromVerse="1" toChapter="20" toVerse="22">
      <aliyah n="2" fromChapter="19" fromVerse="7"/>
      <aliyah n="3" fromChapter="19" fromVerse="14"/>
      <aliyah n="4" fromChapter="19" fromVerse="20"/>
      <aliyah n="5" fromChapter="20" fromVerse="15"/>
    </torah>

  val pesach4torah: Elem =
    <torah book="Exodus" fromChapter="22" fromVerse="24" toChapter="23" toVerse="19">
      <aliyah n="2" fromChapter="22" fromVerse="27"/>
      <aliyah n="3" fromChapter="23" fromVerse="6"/>
    </torah>

  val parshasShekalimMaftir: Elem =
      <maftir book="Exodus" fromChapter="30" fromVerse="11" toVerse="16"/>

  val fastAfternoonTorahPart1: Elem =
    <torah book="Exodus" fromChapter="32" fromVerse="11" toVerse="14">
      <aliyah n="1" fromVerse="11"/>
    </torah>

  val intermediateShabbosTorah: Elem =
    <torah book="Exodus" fromChapter="33" fromVerse="12" toChapter="34" toVerse="26">
      <aliyah n="2" fromChapter="33" fromVerse="17"/>
      <aliyah n="3" fromChapter="33" fromVerse="20"/>
      <aliyah n="4" fromChapter="34" fromVerse="1"/>
      <aliyah n="5" fromChapter="34" fromVerse="4"/>
      <aliyah n="6" fromChapter="34" fromVerse="11"/>
      <aliyah n="7" fromChapter="34" fromVerse="18"/>
    </torah>
}
