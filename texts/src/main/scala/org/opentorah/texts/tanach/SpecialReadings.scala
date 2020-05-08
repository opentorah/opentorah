package org.opentorah.texts.tanach

import scala.xml.Elem

object SpecialReadings {
  val roshHashana1torah: Elem =
    <torah book="Genesis" fromChapter="21" fromVerse="1" toVerse="34">
      <aliyah n="2" fromVerse="5"/>
      <aliyah n="3" fromVerse="9"/>
      <aliyah n="4" fromVerse="13"/>
      <aliyah n="5" fromVerse="18"/>
      <aliyah n="6" fromVerse="22"/>
      <aliyah n="7" fromVerse="28"/>
    </torah>

  val roshHashanah1haftarah: Elem =
    <haftarah book="I Samuel" fromChapter="1" fromVerse="1" toChapter="2" toVerse="10"/>

  val roshHashanah2torah: Elem =
    <torah book="Genesis" fromChapter="22" fromVerse="1" toVerse="24">
      <aliyah n="2" fromVerse="4"/>
      <aliyah n="3" fromVerse="9"/>
      <aliyah n="4" fromVerse="15"/>
      <aliyah n="5" fromVerse="20"/>
    </torah>

  val roshHashanah2haftarah: Elem =
    <haftarah book="Jeremiah" fromChapter="31" fromVerse="1" toVerse="19"/>

  val roshHashanahMaftir: Elem =
    <maftir book="Numbers" fromChapter="29" fromVerse="1" toVerse="6"/>

  val yomKippurTorah: Elem =
    <torah book="Leviticus" fromChapter="16" fromVerse="1" toVerse="34">
      <aliyah n="2" fromVerse="4"/>
      <aliyah n="3" fromVerse="7"/>
      <aliyah n="4" fromVerse="12"/>
      <aliyah n="5" fromVerse="18"/>
      <aliyah n="6" fromVerse="25"/>
      <aliyah n="7" fromVerse="31"/>
    </torah>

  val yomKippurHaftarah: Elem =
    <haftarah book="Isaiah">
      <custom n="Common" fromChapter="57" fromVerse="14" toChapter="58" toVerse="14"/>
      <custom n="Italki, Teiman">
        <part n="1" fromChapter="57" fromVerse="14" toChapter="58" toVerse="14"/>
        <part n="2" fromChapter="59" fromVerse="20" toVerse="21"/>
      </custom>
    </haftarah>

  val yomKippurMaftir: Elem =
    <maftir book="Numbers" fromChapter="29" fromVerse="7" toVerse="11"/>

  val yomKippurAfternoonTorah: Elem =
    <torah book="Leviticus" fromChapter="18" fromVerse="1" toVerse="30">
      <aliyah n="2" fromVerse="6"/>
      <aliyah n="3" fromVerse="22"/>
    </torah>

  val yomKippurAfternoonHaftarah: Elem =
    <haftarah>
      <custom n="Common">
        <part n="1" book="Jonah" fromChapter="1" fromVerse="1" toChapter="4" toVerse="11"/>
        <part n="2" book="Micah" fromChapter="7" fromVerse="18" toVerse="20"/>
      </custom>
      <custom n="Italki">
        <part n="1" book="Obadiah" fromChapter="1" fromVerse="21"/>
        <part n="2" book="Jonah" fromChapter="1" fromVerse="1" toChapter="4" toVerse="11"/>
        <part n="3" book="Micah" fromChapter="7" fromVerse="18" toVerse="20"/>
      </custom>
    </haftarah>

  val succos1and2torah: Elem =
    <torah book="Leviticus" fromChapter="22" fromVerse="26" toChapter="23" toVerse="44">
      <aliyah n="2" fromChapter="23" fromVerse="1"/>
      <aliyah n="3" fromChapter="23" fromVerse="4"/>
      <aliyah n="4" fromChapter="23" fromVerse="9"/>
      <aliyah n="5" fromChapter="23" fromVerse="15"/>
      <aliyah n="6" fromChapter="23" fromVerse="23"/>
      <aliyah n="7" fromChapter="23" fromVerse="33"/>
    </torah>

  val succos1haftarah: Elem =
    <haftarah book="Zechariah" toChapter="14" toVerse="21">
      <custom n="Common" fromChapter="14" fromVerse="1"/>
      <custom n="Teiman" fromChapter="13" fromVerse="9"/>
    </haftarah>

  val succos2haftarah: Elem =
    <haftarah book="I Kings">
      <custom n="Common" fromChapter="8" fromVerse="2" toVerse="21"/>
      <custom n="Italki" fromChapter="7" fromVerse="51" toChapter="8" toVerse="15"/>
      <custom n="Teiman" fromChapter="7" fromVerse="51" toChapter="8" toVerse="21"/>
    </haftarah>

  val succosKorbanot: Elem =
    <torah book="Numbers" fromChapter="29" fromVerse="12" toChapter="30" toVerse="1">
      <aliyah n="2" fromVerse="17"/>
      <aliyah n="3" fromVerse="20"/>
      <aliyah n="4" fromVerse="23"/>
      <aliyah n="5" fromVerse="26"/>
      <aliyah n="6" fromVerse="29"/>
      <aliyah n="7" fromVerse="32"/>
      <aliyah n="8" fromVerse="35"/>
    </torah>

  val succosIntermediateShabbosHaftarah: Elem =
    <haftarah book="Ezekiel" fromChapter="38">
      <custom n="Common" fromVerse="18" toChapter="39" toVerse="16"/>
      <custom n="Italki, Teiman" fromVerse="1" toChapter="38" toVerse="23"/>
    </haftarah>

  /* Artscroll gives custom Ashkenaz ending at 9:1,
     but @michaelko58 insists that it is the same as Sefard and ends at 8:66.
     His explanation: "there are some ashkenazic communities that follow custom Italki.
     It is possible that this is a difference between chassidim and litaim." */
  val sheminiAtzeresHaftarah: Elem =
    <haftarah book="I Kings" fromChapter="8" fromVerse="54">
      <custom n="Common" toChapter="8" toVerse="66"/>
      <custom n="Italki, Chabad" toChapter="9" toVerse="1"/>
    </haftarah>

  val chassanBereishis: Elem =
    <torah book="Genesis" fromChapter="1" fromVerse="1" toChapter="2" toVerse="3">
      <aliyah n="1" fromVerse="1"/>
    </torah>

  val simchasTorahHaftarah: Elem =
    <haftarah book="Joshua">
      <custom n="Common, Italki" fromChapter="1" fromVerse="1" toVerse="18"/>
      <custom n="Teiman">
        <part n="1" fromChapter="1" fromVerse="1" toVerse="9"/>
        <part n="2" fromChapter="6" fromVerse="27"/>
      </custom>
    </haftarah>

  val chanukahFirst: Elem =
    <torah book="Numbers" fromChapter="6" fromVerse="22" toChapter="7" toVerse="11">
      <aliyah n="2" fromChapter="7" fromVerse="1"/>
    </torah>

  val chanukahKorbanotSpans: Elem =
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
    </torah>

  val chanukahShabbos1Haftarah: Elem =
    <haftarah book="Zechariah" fromChapter="2" fromVerse="14" toChapter="4" toVerse="7"/>

  val chanukahShabbos2Haftarah: Elem =
    <haftarah book="I Kings" fromChapter="7" fromVerse="40" toChapter="7">
      <custom n="Common" toVerse="50"/>
      <custom n="Italki" toVerse="51"/>
    </haftarah>

  val parshasShekalimMaftir: Elem =
    <maftir book="Exodus" fromChapter="30" fromVerse="11" toVerse="16"/>

  val parshasShekalimHaftarah: Elem =
    <haftarah book="II Kings">
      <custom n="Ashkenaz, Italki, Teiman, Chabad" fromChapter="12" fromVerse="1" toVerse="17"/>
      <custom n="Sefard" fromChapter="11" fromVerse="17" toChapter="12" toVerse="17"/>
    </haftarah>

  val parshasZachorMaftir: Elem =
    <maftir book="Deuteronomy" fromChapter="25" fromVerse="17" toVerse="19"/>

  val parshasZachorHaftarah: Elem =
    <haftarah book="I Samuel">
      <custom n="Ashkenaz, Chabad" fromChapter="15" fromVerse="2" toVerse="34"/>
      <custom n="Sefard" fromChapter="15" fromVerse="1" toVerse="34"/>
      <custom n="Teiman" fromChapter="14" fromVerse="52" toChapter="15" toVerse="33"/>
    </haftarah>

  val purimTorah: Elem =
    <torah book="Exodus" fromChapter="17" fromVerse="8" toVerse="16">
      <aliyah n="2" fromVerse="11"/>
      <aliyah n="3" fromVerse="14"/>
    </torah>

  val parshasParahMaftir: Elem =
    <maftir book="Numbers" fromChapter="19" fromVerse="1" toVerse="22"/>

  val parshasParahHaftarah: Elem =
    <haftarah book="Ezekiel" fromChapter="36" fromVerse="16">
      <custom n="Ashkenaz, Italki" toVerse="38"/>
      <custom n="Sefard" toVerse="36"/>
    </haftarah>

  val parshasHachodeshMaftir: Elem =
    <maftir book="Exodus" fromChapter="12" fromVerse="1" toVerse="20"/>

  val parshasHachodeshHaftarah: Elem =
    <haftarah book="Ezekiel" fromChapter="45" toChapter="46">
      <custom n="Ashkenaz" fromVerse="16" toVerse="18"/>
      <custom n="Italki" fromVerse="18" toVerse="11"/>
      <custom n="Sefard" fromVerse="18" toVerse="15"/>
      <custom n="Teiman" fromVerse="9" toVerse="11"/>
    </haftarah>

  val shabbosHagodolHaftarah: Elem =
     <haftarah book="Malachi" fromChapter="3" fromVerse="4" toVerse="24"/>

  val pesach1torah: Elem =
    <torah book="Exodus" fromChapter="12" fromVerse="21" toVerse="51">
      <aliyah n="2" fromVerse="25"/>
      <aliyah n="3" fromVerse="29"/>
      <aliyah n="4" fromVerse="33"/>
      <aliyah n="5" fromVerse="37"/>
      <aliyah n="6" fromVerse="43"/>
      <aliyah n="7" fromVerse="48"/>
    </torah>

  val pesachHaftarah: Elem =
    <haftarah book="Joshua">
      <custom n="Ashkenaz, Chabad">
        <part n="1" fromChapter="3" fromVerse="5" toVerse="7"/>
        <part n="2" fromChapter="5" fromVerse="2" toChapter="6" toVerse="1"/>
        <part n="3" fromChapter="6" fromVerse="27"/>
      </custom>
      <custom n="Sefard">
        <part n="1" fromChapter="5" fromVerse="2" toChapter="6" toVerse="1"/>
        <part n="2" fromChapter="6" fromVerse="27"/>
      </custom>
      <custom n="Frankfurt, Hagra" fromChapter="5" fromVerse="2" toChapter="6" toVerse="1"/>
    </haftarah>

  val pesach2Haftarah: Elem =
    <haftarah book="II Kings">
      <custom n="Common">
        <part n="1" fromChapter="23" fromVerse="1" toVerse="9"/>
        <part n="2" fromChapter="23" fromVerse="21" toVerse="25"/>
      </custom>
      <custom n="Italki" fromChapter="23" fromVerse="21" toVerse="30"/>
      <custom n="Teiman">
        <part n="1" fromChapter="22" fromVerse="1" toVerse="7"/>
        <part n="2" fromChapter="23" fromVerse="21" toVerse="25"/>
      </custom>
    </haftarah>

  val pesach3torah: Elem =
    <torah book="Exodus" fromChapter="13" fromVerse="1" toVerse="16">
      <aliyah n="2" fromVerse="5"/>
      <aliyah n="3" fromVerse="11"/>
    </torah>

  val pesach4torah: Elem =
    <torah book="Exodus" fromChapter="22" fromVerse="24" toChapter="23" toVerse="19">
      <aliyah n="2" fromChapter="22" fromVerse="27"/>
      <aliyah n="3" fromChapter="23" fromVerse="6"/>
    </torah>

  val pesach6torah: Elem =
    <torah book="Numbers" fromChapter="9" fromVerse="1" toVerse="14">
      <aliyah n="2" fromVerse="7"/>
      <aliyah n="3" fromVerse="9"/>
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

  val pesach7Hhaftarah: Elem =
    <haftarah book="II Samuel" fromChapter="22" fromVerse="1" toVerse="51"/>

  val pesach8Haftarah: Elem =
    <haftarah book="Isaiah" fromChapter="10" fromVerse="32" toChapter="12" toVerse="6"/>

  val pesachMaftir: Elem =
    <maftir book="Numbers" fromChapter="28" fromVerse="16" toVerse="25"/>

  // Maftir for Pesach Intermediate Shabbos and last two days of Pesach
  val pesachEndMaftir: Elem =
    <maftir book="Numbers" fromChapter="28" fromVerse="19" toVerse="25"/>

  val pesachIntermediateShabbosHaftarah: Elem =
    <haftarah book="Ezekiel">
      <custom n="Common" fromChapter="37" fromVerse="1" toVerse="14"/>
      <custom n="Teiman" fromChapter="36" fromVerse="37" toChapter="37" toVerse="14"/>
    </haftarah>

  val shavuosTorah: Elem =
    <torah book="Exodus" fromChapter="19" fromVerse="1" toChapter="20" toVerse="22">
      <aliyah n="2" fromChapter="19" fromVerse="7"/>
      <aliyah n="3" fromChapter="19" fromVerse="14"/>
      <aliyah n="4" fromChapter="19" fromVerse="20"/>
      <aliyah n="5" fromChapter="20" fromVerse="15"/>
    </torah>

  val shavuosMaftir: Elem =
    <maftir book="Numbers" fromChapter="28" fromVerse="26" toVerse="31"/>

  val shavuosHaftarah: Elem =
    <haftarah book="Ezekiel">
      <custom n="Common">
        <part n="1" fromChapter="1" fromVerse="1" toVerse="28"/>
        <part n="2" fromChapter="3" fromVerse="12"/>
      </custom>
      <custom n="Teiman">
        <part n="1" fromChapter="1" fromVerse="1" toChapter="2" toVerse="2"/>
        <part n="2" fromChapter="3" fromVerse="12"/>
      </custom>
    </haftarah>

  val shavuos2Haftarah: Elem =
    <haftarah book="Habakkuk">
      <custom n="Ashkenaz" fromChapter="3" fromVerse="1" toVerse="19"/>
      <custom n="Sefard" fromChapter="2" fromVerse="20" toChapter="3" toVerse="19"/>
    </haftarah>

  val intermediateShabbosTorah: Elem =
    <torah book="Exodus" fromChapter="33" fromVerse="12" toChapter="34" toVerse="26">
      <aliyah n="2" fromChapter="33" fromVerse="17"/>
      <aliyah n="3" fromChapter="33" fromVerse="20"/>
      <aliyah n="4" fromChapter="34" fromVerse="1"/>
      <aliyah n="5" fromChapter="34" fromVerse="4"/>
      <aliyah n="6" fromChapter="34" fromVerse="11"/>
      <aliyah n="7" fromChapter="34" fromVerse="18"/>
    </torah>

  val festivalEndShabbosTorah: Elem =
    <torah book="Deuteronomy" fromChapter="14" fromVerse="22" toChapter="16" toVerse="17">
      <aliyah n="2" fromChapter="15" fromVerse="1"/>
      <aliyah n="3" fromChapter="15" fromVerse="19"/>
      <aliyah n="4" fromChapter="16" fromVerse="1"/>
      <aliyah n="5" fromChapter="16" fromVerse="4"/>
      <aliyah n="6" fromChapter="16" fromVerse="9"/>
      <aliyah n="7" fromChapter="16" fromVerse="13"/>
    </torah>

  val erevRoshChodeshShabbosHaftarah: Elem =
    <haftarah book="I Samuel" fromChapter="20" fromVerse="18" toVerse="42"/>

  val erevRoshChodeshShabbosAdditionalHaftarah: Elem =
    <haftarah>
      <custom n="Chabad, Fes" book="I Samuel" fromChapter="20">
        <part n="1" fromVerse="18" toVerse="18"/>
        <part n="2" fromVerse="42" toVerse="42"/>
      </custom>
    </haftarah>

  val roshChodesh: Elem =
    <torah book="Numbers" fromChapter="28" fromVerse="1" toVerse="15">
      <aliyah n="2" fromVerse="3"/>
      <aliyah n="3" fromVerse="4"/>
      <aliyah n="4" fromVerse="6"/>
      <aliyah n="5" fromVerse="9"/>
      <aliyah n="6" fromVerse="11"/>
    </torah>

  val roshChodeshShabbosHaftarah: Elem =
    <haftarah book="Isaiah" fromChapter="66">
      <part n="1" fromVerse="1" toVerse="24"/>
      <part n="2" fromVerse="23" toVerse="23"/>
    </haftarah>

  val roshChodeshShabbosAdditionalHaftarah: Elem =
    <haftarah>
      <custom n="Chabad" book="Isaiah" fromChapter="66">
        <part n="1" fromVerse="1" toVerse="1"/>
        <part n="2" fromVerse="23" toVerse="24"/>
        <part n="3" fromVerse="23" toVerse="23"/>
      </custom>
    </haftarah>

  val fastAfternoonTorahPart1: Elem =
    <torah book="Exodus" fromChapter="32" fromVerse="11" toVerse="14">
      <aliyah n="1" fromVerse="11"/>
    </torah>

  val fastDefaultAfternoonHaftarah: Elem =
    <haftarah>
      <custom n="Ashkenaz, Chabad, Morocco" book="Isaiah" fromChapter="55" fromVerse="6" toChapter="56" toVerse="8"/>
      <custom n="Algeria">
        <part n="1" book="Hosea" fromChapter="14" fromVerse="2" toVerse="10"/>
        <part n="2" book="Joel" fromChapter="2" fromVerse="11" toVerse="27"/>
      </custom>
    </haftarah>

  val fasOfGedaliaAfternoonHaftarahExceptions: Elem =
    <haftarah>
      <custom n="Morocco">
        <part n="1" book="Hosea" fromChapter="14" fromVerse="2" toVerse="10"/>
        <part n="2" book="Joel" fromChapter="2" fromVerse="11" toVerse="27"/>
      </custom>
    </haftarah>

  val tishaBeAvTorah: Elem =
    <torah book="Deuteronomy" fromChapter="4" fromVerse="25" toVerse="40">
      <aliyah n="2" fromVerse="30"/>
      <aliyah n="3" fromVerse="36"/>
    </torah>

  val tishaBeAvHaftarah: Elem =
    <haftarah book="Jeremiah">
      <custom n="Common" fromChapter="8" fromVerse="13" toChapter="9" toVerse="23"/>
      <custom n="Teiman">
        <part n="1" fromChapter="6" fromVerse="16" toVerse="17"/>
        <part n="2" fromChapter="8" fromVerse="13" toChapter="9" toVerse="23"/>
      </custom>
    </haftarah>
}
