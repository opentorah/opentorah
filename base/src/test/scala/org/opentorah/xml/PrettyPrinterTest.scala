package org.opentorah.xml

import org.opentorah.util.Effects
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class PrettyPrinterTest extends AnyFlatSpec with Matchers {

  private def render(from: From, width: Int): String =
    new PrettyPrinter(
      width,
      indent = 2,
      clingyElements = Set("note")
    ).render(ScalaXml)(Effects.unsafeRun(from.loadTask))

  private def check(from: From, width: Int, expected: String): Unit = {
    val result = render(from, width)
    result shouldBe expected.stripMargin + "\n"
  }

  private def check(xml: ScalaXml.Element, width: Int, expected: String): Unit =
    check(From.xml("test XML", xml), width, expected)

  "Chunking" should "work" in {
    check(<a>X<b/> </a>, 120, expected =
    """|<a>X<b/></a>""")

    check(<a><b/><b/><b/></a>, 120, expected =
      """|<a>
         |  <b/>
         |  <b/>
         |  <b/>
         |</a>""")

    check(<a><b/><b/><b/><b/></a>, 120, expected =
      """|<a>
         |  <b/>
         |  <b/>
         |  <b/>
         |  <b/>
         |</a>""")
  }

  "PrettyPrinter" should "work" in {
    check(<a><b></b></a>, 4, expected =
      """|<a>
         |<b/>
         |</a>""")

    check(<a><b></b><b></b></a>, 4, expected =
      """|<a>
         |  <b
         |  />
         |  <b
         |  />
         |</a>""")

    check(<el/>, 5, """<el/>""")

    check(<el/>, 4, expected =
      """|<el
         |/>""")

    check(<el a1="v1" a2="v2"/>, 120, """<el a1="v1" a2="v2"/>""")

    check(<el a1="v1" a2="v2"/>, 21, """<el a1="v1" a2="v2"/>""")

    check(<el a1="v1" a2="v2"/>, 20, expected =
      """|<el a1="v1" a2="v2"
         |/>""")

    check(<el a1="v1" a2="v2"/>, 18, expected =
      """|<el a1="v1" a2=
         |"v2"/>""".stripMargin)

    check(<el a1="v1" a2="v2"/>, 10, expected =
      """|<el a1=
         |"v1" a2=
         |"v2"/>""")

    check(<creation><date when="2020-02-24"/><note/></creation>, 53,
      expected = """<creation><date when="2020-02-24"/><note/></creation>""")

    check(<creation><date when="2020-02-24"/><note/></creation>, 52, expected =
        """|<creation><date when="2020-02-24"/><note/>
           |</creation>""")

    check(<creation><date when="2020-02-24"/><note/></creation>, 33, expected =
      """|<creation><date when="2020-02-24"
         |/><note/></creation>""")

    check(<creation><date when="2020-02-24"/><note/></creation>, 24, expected =
      """|<creation><date when=
         |"2020-02-24"/><note/>
         |</creation>""")

    check(<creation><date when="2020-02-24"/>blah</creation>, 50,
      expected = """<creation><date when="2020-02-24"/>blah</creation>""")

    check(<creation><date when="2020-02-24"/>blah</creation>, 49, expected =
      """|<creation><date when="2020-02-24"
         |/>blah</creation>""")

    check(<creation><date when="2020-02-24"/>blah</creation>, 30, expected =
      """|<creation><date when=
         |"2020-02-24"/>blah</creation>""")

    check(<creation><date when="2020-02-24"/>blah</creation>, 24, expected =
      """|<creation><date when=
         |"2020-02-24"
         |/>blah</creation>""")

    check(<p><l>line1</l>
      <!-- comment --><l>line2</l></p>, 30, expected =
      """|<p>
         |  <l>line1</l>
         |  <!-- comment -->
         |  <l>line2</l>
         |</p>""")

    check(
      <xsl:stylesheet xmlns:xsl={Xsl.namespace.uri} version={Xsl.version(false)}
                      xmlns:db="http://docbook.org/ns/docbook"
                      exclude-result-prefixes="db">
        <!-- Customizations go here. -->
        <!-- dummy -->
      </xsl:stylesheet>,
      width = 120, expected =
        s"""|<xsl:stylesheet xmlns:db="http://docbook.org/ns/docbook" xmlns:xsl="${Xsl.namespace.uri}" version="1.0"
            |exclude-result-prefixes="db">
            |  <!-- Customizations go here. -->
            |  <!-- dummy -->
            |</xsl:stylesheet>""")

    check(
      <xsl:stylesheet xmlns:xsl={Xsl.namespace.uri} version={Xsl.version(false)}
                      xmlns:db="http://docbook.org/ns/docbook"
                      exclude-result-prefixes="db">
        <xsl:param value="x"/>
        <d:y xmlns:y="zzz"/>
      </xsl:stylesheet>,
      width = 120, expected =
        s"""|<xsl:stylesheet xmlns:db="http://docbook.org/ns/docbook" xmlns:xsl="${Xsl.namespace.uri}" version="1.0"
            |exclude-result-prefixes="db">
            |  <xsl:param value="x"/>
            |  <d:y xmlns:y="zzz"/>
            |</xsl:stylesheet>""")
  }

  "Encoding" should "work" in {
    check(<ref target="https://forum.j-roots.info/viewtopic.php?t=5970&amp;start=240">text</ref>, 120,
    s"""<ref target="https://forum.j-roots.info/viewtopic.php?t=5970&amp;start=240">text</ref>"""
    )
  }
}
