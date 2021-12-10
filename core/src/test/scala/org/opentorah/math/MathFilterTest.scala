package org.opentorah.math

import org.opentorah.docbook.DocBook
import org.opentorah.xml.{Dom, XInclude, Xml}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class MathFilterTest extends AnyFlatSpecLike, Matchers:

  it should "work for wrapped display TeX" in {
    // Serializer outputs UTF-16; xml namespace is made explicit; order of attributes and spacing are different -
    // but other than that, the document is the same.
    parse(
     s"""|${Xml.header16}
         |<article xml:id="test-id" xmlns="${DocBook.namespace.uri}" version="${DocBook.version}" xmlns:xi="${XInclude.namespace.uri}">
         |  <para>
         |    Wrapped display TeX:<informalequation>
         |    <math xmlns="${MathML.namespace.uri}"
         |          xmlns:mathjax="${MathFilter.namespace.uri}" mathjax:input="TeX">
         |      <mrow><mi>x = {-b \\pm \\sqrt{b^2-4ac} \\over 2a}.</mi></mrow>
         |    </math></informalequation>
         |  </para>
         |</article>""".stripMargin
    ) shouldBe
     s"""|${Xml.header}
         |<article xmlns="${DocBook.namespace.uri}" xmlns:xi="${XInclude.namespace.uri}" version="${DocBook.version}" xml:id="test-id"
         |>
         |  <para>Wrapped display TeX:<informalequation>
         |    <math xmlns="${MathML.namespace.uri}" xmlns:mathjax="${MathFilter.namespace.uri}" display="block"
         |    mathjax:input="TeX">
         |      <mrow>
         |        <mi>x = {-b \\pm \\sqrt{b^2-4ac} \\over 2a}.</mi>
         |      </mrow>
         |    </math>
         |  </informalequation></para>
         |</article>
         |""".stripMargin
  }

  it should "work for display TeX" in {
    parse(
     s"""|${Xml.header}
         |<article xml:id="test-id" xmlns="${DocBook.namespace.uri}" version="${DocBook.version}" xmlns:xi="${XInclude.namespace.uri}">
         |  <para>Display TeX:$$$$x = {-b \\pm \\sqrt{b^2-4ac} \\over 2a}.$$$$</para>
         |</article>""".stripMargin
    ) shouldBe
     s"""|${Xml.header}
         |<article xmlns="${DocBook.namespace.uri}" xmlns:xi="${XInclude.namespace.uri}" version="${DocBook.version}" xml:id="test-id"
         |>
         |  <para>Display TeX:<informalequation>
         |    <math xmlns="${MathML.namespace.uri}" xmlns:mathjax="${MathFilter.namespace.uri}" mathjax:input=
         |    "TeX">
         |      <mrow>
         |        <mi>x = {-b \\pm \\sqrt{b^2-4ac} \\over 2a}.</mi>
         |      </mrow>
         |    </math>
         |  </informalequation></para>
         |</article>
         |""".stripMargin
  }

  it should "work for inline TeX" in {
    parse(
     s"""|${Xml.header}
         |<article xml:id="test-id" xmlns="${DocBook.namespace.uri}" version="${DocBook.version}" xmlns:xi="${XInclude.namespace.uri}">
         |  <para>Inline TeX:$$x = {-b \\pm \\sqrt{b^2-4ac} \\over 2a}.$$</para>
         |</article>
         |""".stripMargin
    ) shouldBe
     s"""|${Xml.header}
         |<article xmlns="${DocBook.namespace.uri}" xmlns:xi="${XInclude.namespace.uri}" version="${DocBook.version}" xml:id="test-id"
         |>
         |  <para>Inline TeX:<inlineequation>
         |    <math xmlns="${MathML.namespace.uri}" xmlns:mathjax="${MathFilter.namespace.uri}" mathjax:input=
         |    "inline-TeX">
         |      <mrow>
         |        <mi>x = {-b \\pm \\sqrt{b^2-4ac} \\over 2a}.</mi>
         |      </mrow>
         |    </math>
         |  </inlineequation></para>
         |</article>
         |""".stripMargin
  }

  it should "work for equation display TeX" in {
    parse(
     s"""|${Xml.header16}
         |<article xmlns="${DocBook.namespace.uri}" version="${DocBook.version}" xml:id="test-id" xmlns:xi="${XInclude.namespace.uri}">
         |  <para>Explicit display TeX:<equation>$$$$x = {-b \\pm \\sqrt{b^2-4ac} \\over 2a}.$$$$</equation></para>
         |</article>""".stripMargin
    ) shouldBe
     s"""|${Xml.header}
         |<article xmlns="${DocBook.namespace.uri}" xmlns:xi="${XInclude.namespace.uri}" version="${DocBook.version}" xml:id="test-id"
         |>
         |  <para>Explicit display TeX:<equation>
         |    <math xmlns="${MathML.namespace.uri}" xmlns:mathjax="${MathFilter.namespace.uri}" mathjax:input=
         |    "TeX">
         |      <mrow>
         |        <mi>x = {-b \\pm \\sqrt{b^2-4ac} \\over 2a}.</mi>
         |      </mrow>
         |    </math>
         |  </equation></para>
         |</article>
         |""".stripMargin
  }

  private def parse(string: String): String =
    val element: Dom.Element = Dom.loadFromString(string, filters = Seq(
        MathConfiguration.default.mathFilter
//        , new org.opentorah.xml.TracingFilter
      )
    )
    val result: String = DocBook.prettyPrinter.renderWithHeader(Dom)(element)
//    println(result)
    result
