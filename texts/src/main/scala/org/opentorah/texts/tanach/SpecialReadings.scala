package org.opentorah.texts.tanach

import org.opentorah.xml.{From, Parsable, Parser}
import scala.xml.Elem

object SpecialReadings {
  def parse[R](parsable: Parsable[R], what: String, element: Elem): R =
    Parser.parseDo(parsable.parse(From.xml(what, element)))

  def parseTorah(element: Elem): Torah = parse(Torah.torahParsable, "Torah", element)

  def parseMaftir(element: Elem): Torah.Maftir = parse(Torah.maftirParsable, "Maftir", element)

  def parseHaftarah(element: Elem, full: Boolean = true): Haftarah.Customs =
    parse(Haftarah.haftarahParsable(full), "Haftarah", element)

  object ErevRoshChodesh {
    val shabbosHaftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="I Samuel" fromChapter="20" fromVerse="18" toVerse="42"/>)

    val shabbosAdditionalHaftarah: Haftarah.Customs = parseHaftarah(full = false, element =
      <haftarah>
        <custom n="Chabad, Fes" book="I Samuel" fromChapter="20">
          <part n="1" fromVerse="18" toVerse="18"/>
          <part n="2" fromVerse="42" toVerse="42"/>
        </custom>
      </haftarah>)
  }

  object RoshChodesh {
    val torah: Torah = parseTorah(
      <torah book="Numbers" fromChapter="28" fromVerse="1" toVerse="15">
        <aliyah n="2" fromVerse="3"/>
        <aliyah n="3" fromVerse="4"/>
        <aliyah n="4" fromVerse="6"/>
        <aliyah n="5" fromVerse="9"/>
        <aliyah n="6" fromVerse="11"/>
      </torah>)

    val shabbosHaftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Isaiah" fromChapter="66">
        <part n="1" fromVerse="1" toVerse="24"/>
        <part n="2" fromVerse="23" toVerse="23"/>
      </haftarah>)

    val shabbosAdditionalHaftarah: Haftarah.Customs = parseHaftarah(full = false, element =
      <haftarah>
        <custom n="Chabad" book="Isaiah" fromChapter="66">
          <part n="1" fromVerse="1" toVerse="1"/>
          <part n="2" fromVerse="23" toVerse="24"/>
          <part n="3" fromVerse="23" toVerse="23"/>
        </custom>
      </haftarah>)
  }

  object FestivalEnd {
    val shabbosTorah: Torah = parseTorah(
      <torah book="Deuteronomy" fromChapter="14" fromVerse="22" toChapter="16" toVerse="17">
        <aliyah n="2" fromChapter="15" fromVerse="1"/>
        <aliyah n="3" fromChapter="15" fromVerse="19"/>
        <aliyah n="4" fromChapter="16" fromVerse="1"/>
        <aliyah n="5" fromChapter="16" fromVerse="4"/>
        <aliyah n="6" fromChapter="16" fromVerse="9"/>
        <aliyah n="7" fromChapter="16" fromVerse="13"/>
      </torah>)

    val weekdayTorah: Torah = Torah(shabbosTorah.spans.drop(2))

    val sheminiAtzeresWeekdayTorah: Torah = shabbosTorah.drop(Set(2, 3)) // TODO move to sheminiAtzeres
  }

  object IntermediateShabbos {
    val torah: Torah = parseTorah(
      <torah book="Exodus" fromChapter="33" fromVerse="12" toChapter="34" toVerse="26">
        <aliyah n="2" fromChapter="33" fromVerse="17"/>
        <aliyah n="3" fromChapter="33" fromVerse="20"/>
        <aliyah n="4" fromChapter="34" fromVerse="1"/>
        <aliyah n="5" fromChapter="34" fromVerse="4"/>
        <aliyah n="6" fromChapter="34" fromVerse="11"/>
        <aliyah n="7" fromChapter="34" fromVerse="18"/>
      </torah>)
  }

  object RoshHashanah {
    val torah1shabbos: Torah = parseTorah(
      <torah book="Genesis" fromChapter="21" fromVerse="1" toVerse="34">
        <aliyah n="2" fromVerse="5"/>
        <aliyah n="3" fromVerse="9"/>
        <aliyah n="4" fromVerse="13"/>
        <aliyah n="5" fromVerse="18"/>
        <aliyah n="6" fromVerse="22"/>
        <aliyah n="7" fromVerse="28"/>
      </torah>)

    val torah1weekday: Torah = torah1shabbos.drop(Set(3, 5))

    val haftarah1: Haftarah.Customs = parseHaftarah(
      <haftarah book="I Samuel" fromChapter="1" fromVerse="1" toChapter="2" toVerse="10"/>)

    val torah2: Torah = parseTorah(
      <torah book="Genesis" fromChapter="22" fromVerse="1" toVerse="24">
        <aliyah n="2" fromVerse="4"/>
        <aliyah n="3" fromVerse="9"/>
        <aliyah n="4" fromVerse="15"/>
        <aliyah n="5" fromVerse="20"/>
      </torah>)

    val haftarah2: Haftarah.Customs = parseHaftarah(
      <haftarah book="Jeremiah" fromChapter="31" fromVerse="1" toVerse="19"/>)

    val maftir: Torah.Maftir = parseMaftir(
      <maftir book="Numbers" fromChapter="29" fromVerse="1" toVerse="6"/>)
  }

  object YomKippur {
    val torahShabbos: Torah = parseTorah(
      <torah book="Leviticus" fromChapter="16" fromVerse="1" toVerse="34">
        <aliyah n="2" fromVerse="4"/>
        <aliyah n="3" fromVerse="7"/>
        <aliyah n="4" fromVerse="12"/>
        <aliyah n="5" fromVerse="18"/>
        <aliyah n="6" fromVerse="25"/>
        <aliyah n="7" fromVerse="31"/>
      </torah>)

    val torahWeekday: Torah = torahShabbos.drop(Set(2))

    val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Isaiah">
        <custom n="Common" fromChapter="57" fromVerse="14" toChapter="58" toVerse="14"/>
        <custom n="Italki, Teiman">
          <part n="1" fromChapter="57" fromVerse="14" toChapter="58" toVerse="14"/>
          <part n="2" fromChapter="59" fromVerse="20" toVerse="21"/>
        </custom>
      </haftarah>)

    val maftir: Torah.Maftir = parseMaftir(
      <maftir book="Numbers" fromChapter="29" fromVerse="7" toVerse="11"/>)

    val afternoonTorah: Torah = parseTorah(
      <torah book="Leviticus" fromChapter="18" fromVerse="1" toVerse="30">
        <aliyah n="2" fromVerse="6"/>
        <aliyah n="3" fromVerse="22"/>
      </torah>)

    val afternoonHaftarah: Haftarah.Customs = parseHaftarah(
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
      </haftarah>)
  }

  object Succos {
    val torah1and2Shabbos: Torah = parseTorah(
      <torah book="Leviticus" fromChapter="22" fromVerse="26" toChapter="23" toVerse="44">
        <aliyah n="2" fromChapter="23" fromVerse="1"/>
        <aliyah n="3" fromChapter="23" fromVerse="4"/>
        <aliyah n="4" fromChapter="23" fromVerse="9"/>
        <aliyah n="5" fromChapter="23" fromVerse="15"/>
        <aliyah n="6" fromChapter="23" fromVerse="23"/>
        <aliyah n="7" fromChapter="23" fromVerse="33"/>
      </torah>)

    val torah1and2Weekday: Torah = torah1and2Shabbos.drop(Set(2, 4))

    val haftarah1: Haftarah.Customs = parseHaftarah(
      <haftarah book="Zechariah" toChapter="14" toVerse="21">
        <custom n="Common" fromChapter="14" fromVerse="1"/>
        <custom n="Teiman" fromChapter="13" fromVerse="9"/>
      </haftarah>)

    val haftarah2: Haftarah.Customs = parseHaftarah(
      <haftarah book="I Kings">
        <custom n="Common" fromChapter="8" fromVerse="2" toVerse="21"/>
        <custom n="Italki" fromChapter="7" fromVerse="51" toChapter="8" toVerse="15"/>
        <custom n="Teiman" fromChapter="7" fromVerse="51" toChapter="8" toVerse="21"/>
      </haftarah>)

    val korbanot: Torah = parseTorah(
      <torah book="Numbers" fromChapter="29" fromVerse="12" toChapter="30" toVerse="1">
        <aliyah n="2" fromVerse="17"/>
        <aliyah n="3" fromVerse="20"/>
        <aliyah n="4" fromVerse="23"/>
        <aliyah n="5" fromVerse="26"/>
        <aliyah n="6" fromVerse="29"/>
        <aliyah n="7" fromVerse="32"/>
        <aliyah n="8" fromVerse="35"/>
      </torah>)

    val intermediateShabbosHaftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Ezekiel" fromChapter="38">
        <custom n="Common" fromVerse="18" toChapter="39" toVerse="16"/>
        <custom n="Italki, Teiman" fromVerse="1" toChapter="38" toVerse="23"/>
      </haftarah>)

    /* Artscroll gives custom Ashkenaz ending at 9:1,
     but @michaelko58 insists that it is the same as Sefard and ends at 8:66.
     His explanation: "there are some ashkenazic communities that follow custom Italki.
     It is possible that this is a difference between chassidim and litaim." */
    val sheminiAtzeresHaftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="I Kings" fromChapter="8" fromVerse="54">
        <custom n="Common" toChapter="8" toVerse="66"/>
        <custom n="Italki, Chabad" toChapter="9" toVerse="1"/>
      </haftarah>)

    val chassanBereishis: Torah = parseTorah(
      <torah book="Genesis" fromChapter="1" fromVerse="1" toChapter="2" toVerse="3">
        <aliyah n="1" fromVerse="1"/>
      </torah>)

    val simchasTorahHaftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Joshua">
        <custom n="Common, Italki" fromChapter="1" fromVerse="1" toVerse="18"/>
        <custom n="Teiman">
          <part n="1" fromChapter="1" fromVerse="1" toVerse="9"/>
          <part n="2" fromChapter="6" fromVerse="27"/>
        </custom>
      </haftarah>)
  }

  object Chanukah {
    val first: Torah = parseTorah(
      <torah book="Numbers" fromChapter="6" fromVerse="22" toChapter="7" toVerse="11">
        <aliyah n="2" fromChapter="7" fromVerse="1"/>
      </torah>)

    val korbanot: Torah = parseTorah(
      <torah book="Numbers" fromChapter="7" fromVerse="12" toChapter="8" toVerse="4">
        <aliyah n="1" fromVerse="12"/>
        <aliyah n="2" fromVerse="15"/>
        <aliyah n="3" fromVerse="18"/>
        <aliyah n="4" fromVerse="21"/>
        <aliyah n="5" fromVerse="24"/>
        <aliyah n="6" fromVerse="27"/>
        <aliyah n="7" fromVerse="30"/>
        <aliyah n="8" fromVerse="33"/>
        <aliyah n="9" fromVerse="36"/>
        <aliyah n="10" fromVerse="39"/>
        <aliyah n="11" fromVerse="42"/>
        <aliyah n="12" fromVerse="45"/>
        <aliyah n="13" fromVerse="48"/>
        <aliyah n="14" fromVerse="51"/>
        <aliyah n="15" fromVerse="54"/>
        <aliyah n="16" fromVerse="57"/>
        <aliyah n="17" fromVerse="60"/>
      </torah>)

    val shabbos1Haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Zechariah" fromChapter="2" fromVerse="14" toChapter="4" toVerse="7"/>)

    val shabbos2Haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="I Kings" fromChapter="7" fromVerse="40" toChapter="7">
        <custom n="Common" toVerse="50"/>
        <custom n="Italki" toVerse="51"/>
      </haftarah>)
  }

  object ParshasShekalim {
    val maftir: Torah.Maftir = parseMaftir(
      <maftir book="Exodus" fromChapter="30" fromVerse="11" toVerse="16"/>)

    val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="II Kings">
        <custom n="Ashkenaz, Italki, Teiman, Chabad" fromChapter="12" fromVerse="1" toVerse="17"/>
        <custom n="Sefard" fromChapter="11" fromVerse="17" toChapter="12" toVerse="17"/>
      </haftarah>)
  }

  object ParshasZachor {
    val maftir: Torah.Maftir = parseMaftir(
      <maftir book="Deuteronomy" fromChapter="25" fromVerse="17" toVerse="19"/>)

    val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="I Samuel">
        <custom n="Ashkenaz, Chabad" fromChapter="15" fromVerse="2" toVerse="34"/>
        <custom n="Sefard" fromChapter="15" fromVerse="1" toVerse="34"/>
        <custom n="Teiman" fromChapter="14" fromVerse="52" toChapter="15" toVerse="33"/>
      </haftarah>)
  }

  object Purim {
    val torah: Torah = parseTorah(
      <torah book="Exodus" fromChapter="17" fromVerse="8" toVerse="16">
        <aliyah n="2" fromVerse="11"/>
        <aliyah n="3" fromVerse="14"/>
      </torah>)
  }

  object ParshasParah {
    val maftir: Torah.Maftir = parseMaftir(
      <maftir book="Numbers" fromChapter="19" fromVerse="1" toVerse="22"/>)

    val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Ezekiel" fromChapter="36" fromVerse="16">
        <custom n="Ashkenaz, Italki" toVerse="38"/>
        <custom n="Sefard" toVerse="36"/>
      </haftarah>)
  }

  object ParshasHachodesh {
    val maftir: Torah.Maftir = parseMaftir(
      <maftir book="Exodus" fromChapter="12" fromVerse="1" toVerse="20"/>)

    val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Ezekiel" fromChapter="45" toChapter="46">
        <custom n="Ashkenaz" fromVerse="16" toVerse="18"/>
        <custom n="Italki" fromVerse="18" toVerse="11"/>
        <custom n="Sefard" fromVerse="18" toVerse="15"/>
        <custom n="Teiman" fromVerse="9" toVerse="11"/>
      </haftarah>)
  }

  object ShabbosHagodol {
    val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Malachi" fromChapter="3" fromVerse="4" toVerse="24"/>)
  }

  object Pesach {
    val torah1Shabbos: Torah = parseTorah(
      <torah book="Exodus" fromChapter="12" fromVerse="21" toVerse="51">
        <aliyah n="2" fromVerse="25"/>
        <aliyah n="3" fromVerse="29"/>
        <aliyah n="4" fromVerse="33"/>
        <aliyah n="5" fromVerse="37"/>
        <aliyah n="6" fromVerse="43"/>
        <aliyah n="7" fromVerse="48"/>
      </torah>)

    val torah1Weekday: Torah = torah1Shabbos.drop(Set(4, 7))

    val maftir: Torah.Maftir = parseMaftir(
        <maftir book="Numbers" fromChapter="28" fromVerse="16" toVerse="25"/>)

    val haftarah1: Haftarah.Customs = parseHaftarah(
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
      </haftarah>)

    val haftarah2: Haftarah.Customs = parseHaftarah(
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
      </haftarah>)

    val torah3: Torah = parseTorah(
      <torah book="Exodus" fromChapter="13" fromVerse="1" toVerse="16">
        <aliyah n="2" fromVerse="5"/>
        <aliyah n="3" fromVerse="11"/>
      </torah>)

    val torah4: Torah = parseTorah(
      <torah book="Exodus" fromChapter="22" fromVerse="24" toChapter="23" toVerse="19">
        <aliyah n="2" fromChapter="22" fromVerse="27"/>
        <aliyah n="3" fromChapter="23" fromVerse="6"/>
      </torah>)

    val torah6: Torah = parseTorah(
      <torah book="Numbers" fromChapter="9" fromVerse="1" toVerse="14">
        <aliyah n="2" fromVerse="7"/>
        <aliyah n="3" fromVerse="9"/>
      </torah>)

    val torah7Shabbos: Torah = parseTorah(
      <torah book="Exodus" fromChapter="13" fromVerse="17" toChapter="15" toVerse="26">
        <aliyah n="2" fromChapter="13" fromVerse="20"/>
        <aliyah n="3" fromChapter="14" fromVerse="1"/>
        <aliyah n="4" fromChapter="14" fromVerse="5"/>
        <aliyah n="5" fromChapter="14" fromVerse="9"/>
        <aliyah n="6" fromChapter="14" fromVerse="15"/>
        <aliyah n="7" fromChapter="14" fromVerse="26"/>
      </torah>)

    val torah7Weekday: Torah = torah7Shabbos.drop(Set(2, 4))

    val haftarah7: Haftarah.Customs = parseHaftarah(
      <haftarah book="II Samuel" fromChapter="22" fromVerse="1" toVerse="51"/>)

    val haftarah8: Haftarah.Customs = parseHaftarah(
      <haftarah book="Isaiah" fromChapter="10" fromVerse="32" toChapter="12" toVerse="6"/>)

    // Maftir for Pesach Intermediate Shabbos and last two days of Pesach
    val maftirEnd: Torah.Maftir = parseMaftir(
      <maftir book="Numbers" fromChapter="28" fromVerse="19" toVerse="25"/>)

    val intermediateShabbosHaftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Ezekiel">
        <custom n="Common" fromChapter="37" fromVerse="1" toVerse="14"/>
        <custom n="Teiman" fromChapter="36" fromVerse="37" toChapter="37" toVerse="14"/>
      </haftarah>)
  }

  object Shavuos {
    val torah: Torah = parseTorah(
      <torah book="Exodus" fromChapter="19" fromVerse="1" toChapter="20" toVerse="22">
        <aliyah n="2" fromChapter="19" fromVerse="7"/>
        <aliyah n="3" fromChapter="19" fromVerse="14"/>
        <aliyah n="4" fromChapter="19" fromVerse="20"/>
        <aliyah n="5" fromChapter="20" fromVerse="15"/>
      </torah>)

    val maftir: Torah.Maftir = parseMaftir(
      <maftir book="Numbers" fromChapter="28" fromVerse="26" toVerse="31"/>)

    val haftarah1: Haftarah.Customs = parseHaftarah(
      <haftarah book="Ezekiel">
        <custom n="Common">
          <part n="1" fromChapter="1" fromVerse="1" toVerse="28"/>
          <part n="2" fromChapter="3" fromVerse="12"/>
        </custom>
        <custom n="Teiman">
          <part n="1" fromChapter="1" fromVerse="1" toChapter="2" toVerse="2"/>
          <part n="2" fromChapter="3" fromVerse="12"/>
        </custom>
      </haftarah>)

    val haftarah2: Haftarah.Customs = parseHaftarah(
      <haftarah book="Habakkuk">
        <custom n="Ashkenaz" fromChapter="3" fromVerse="1" toVerse="19"/>
        <custom n="Sefard" fromChapter="2" fromVerse="20" toChapter="3" toVerse="19"/>
      </haftarah>)
  }

  object Fast {
    val afternoonTorahPart1: Torah = parseTorah(
      <torah book="Exodus" fromChapter="32" fromVerse="11" toVerse="14">
        <aliyah n="1" fromVerse="11"/>
      </torah>)

    val defaultAfternoonHaftarah: Haftarah.Customs = parseHaftarah(full = false, element =
      <haftarah>
        <custom n="Ashkenaz, Chabad, Morocco" book="Isaiah" fromChapter="55" fromVerse="6" toChapter="56" toVerse="8"/>
        <custom n="Algeria">
          <part n="1" book="Hosea" fromChapter="14" fromVerse="2" toVerse="10"/>
          <part n="2" book="Joel" fromChapter="2" fromVerse="11" toVerse="27"/>
        </custom>
      </haftarah>)
  }

  object FastOfGedalia {
    val afternoonHaftarahExceptions: Haftarah.Customs = parseHaftarah(full = false, element =
      <haftarah>
        <custom n="Morocco">
          <part n="1" book="Hosea" fromChapter="14" fromVerse="2" toVerse="10"/>
          <part n="2" book="Joel" fromChapter="2" fromVerse="11" toVerse="27"/>
        </custom>
      </haftarah>)
  }

  object TishaBeAv {
    val torah: Torah = parseTorah(
      <torah book="Deuteronomy" fromChapter="4" fromVerse="25" toVerse="40">
        <aliyah n="2" fromVerse="30"/>
        <aliyah n="3" fromVerse="36"/>
      </torah>)

    val haftarah: Haftarah.Customs = parseHaftarah(
      <haftarah book="Jeremiah">
        <custom n="Common" fromChapter="8" fromVerse="13" toChapter="9" toVerse="23"/>
        <custom n="Teiman">
          <part n="1" fromChapter="6" fromVerse="16" toVerse="17"/>
          <part n="2" fromChapter="8" fromVerse="13" toChapter="9" toVerse="23"/>
        </custom>
      </haftarah>)
  }
}
