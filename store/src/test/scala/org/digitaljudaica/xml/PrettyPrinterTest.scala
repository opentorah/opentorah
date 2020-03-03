package org.digitaljudaica.xml

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.xml.Elem

final class PrettyPrinterTest extends AnyFlatSpec with Matchers {

  private val indent: Int = 2
  private val width: Int = 120

  private def formatStandard(from: From): String =
    new scala.xml.PrettyPrinter(width, 2).format(Parser.run(from.load))

  // TODO see that PaigesPrettyPrinter doesn't lose anything
  // (compare serialization of the input and of the parsed pretty-print...).

  private def render(from: From, width: Int): String =
    new PaigesPrettyPrinter(width, indent).render(Parser.run(from.load))

  private def check(from: From, width: Int, expected: String): Unit = {
    val result = render(from, width)
    result shouldBe expected.stripMargin
  }

  private def check(xml: Elem, width: Int, expected: String): Unit =
    check(From.xml(xml), width, expected)

  private def print(from: From, width: Int): Unit = {
//    println(new PaigesPrettyPrinter(indent, width)
//      .fromNode(Parser.run(from.load))
//      .representation(true)
//      .render(120)
//    )

    println("                                                                                                                                   ".take(width-1) + "|")
    println("0         1         2         3         4         5         6         7         8         9         0         1         2         3")
    println("01234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890")
    println(render(from, width))
  }

  "Chunking" should "work" in {
    print(From.resource(this, "rgada"), 90)
  }

  "PaigesPrettyPrinter" should "work" in {
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
      """|<el a1="v1"
         |a2="v2"/>""")

    check(<el a1="v1" a2="v2"/>, 10, expected =
      """|<el
         |a1="v1"
         |a2="v2"/>""")

    check(<creation><date when="2020-02-24"/><note/></creation>, 53,
      expected = """<creation><date when="2020-02-24"/><note/></creation>""")

    check(<creation><date when="2020-02-24"/><note/></creation>, 52, expected =
        """|<creation><date when="2020-02-24"/><note/>
           |</creation>""")

    check(<creation><date when="2020-02-24"/><note/></creation>, 33, expected =
      """|<creation><date when="2020-02-24"
         |/><note/></creation>""")

    check(<creation><date when="2020-02-24"/><note/></creation>, 24, expected =
      """|<creation><date
         |when="2020-02-24"/><note
         |/></creation>""")

    check(<creation><date when="2020-02-24"/>blah</creation>, 50,
      expected = """<creation><date when="2020-02-24"/>blah</creation>""")

    check(<creation><date when="2020-02-24"/>blah</creation>, 49, expected =
      """|<creation><date when="2020-02-24"/>blah
         |</creation>""")

    check(<creation><date when="2020-02-24"/>blah</creation>, 30, expected = // there is a space after <date.
      """|<creation><date
         |when="2020-02-24"/>blah
         |</creation>""")

    check(<creation><date when="2020-02-24"/>blah</creation>, 24, expected = // there is a space after <date.
      """|<creation><date
         |when="2020-02-24"/>blah
         |</creation>""")
  }
}
