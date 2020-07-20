package org.opentorah.docbook

import org.opentorah.docbook.plugin.MathFilter
import org.opentorah.mathjax.{Configuration, MathJax}
import org.opentorah.xml.{Namespace, PrettyPrinter, Saxon, Xerces, Xml}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.w3c.dom.{Document, Node}

class MathFilterTest extends AnyFlatSpecLike with Matchers {

  it should "work for wrapped display TeX" in {
    // Serializer outputs UTF-16; xml namespace is made explicit; order of attributes and spacing are different -
    // but other than that, the document is the same.
    parse(
     s"""|${Namespace.Xml.header16}
         |<article xml:id="test-id" xmlns="${Namespace.DocBook.uri}" version="${Namespace.DocBook.version}" xmlns:xi="${Namespace.XInclude.uri}">
         |  <para>
         |    Wrapped display TeX:<informalequation>
         |    <math xmlns="${Namespace.MathML.uri}"
         |          xmlns:mathjax="${MathJax.Namespace.uri}" mathjax:input="TeX">
         |      <mrow><mi>x = {-b \\pm \\sqrt{b^2-4ac} \\over 2a}.</mi></mrow>
         |    </math></informalequation>
         |  </para>
         |</article>""".stripMargin
    ) shouldBe
     s"""|${Namespace.Xml.header}
         |<article version="${Namespace.DocBook.version}" xml:id="test-id" xmlns="${Namespace.DocBook.uri}" xmlns:xi="${Namespace.XInclude.uri}"
         |>
         |  <para>Wrapped display TeX:<informalequation>
         |    <math display="block" mathjax:input="TeX" xmlns="${Namespace.MathML.uri}" xmlns:mathjax=
         |    "${MathJax.Namespace.uri}">
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
     s"""|${Namespace.Xml.header}
         |<article xml:id="test-id" xmlns="${Namespace.DocBook.uri}" version="${Namespace.DocBook.version}" xmlns:xi="${Namespace.XInclude.uri}">
         |  <para>Display TeX:$$$$x = {-b \\pm \\sqrt{b^2-4ac} \\over 2a}.$$$$</para>
         |</article>""".stripMargin
    ) shouldBe
     s"""|${Namespace.Xml.header}
         |<article version="${Namespace.DocBook.version}" xml:id="test-id" xmlns="${Namespace.DocBook.uri}" xmlns:xi="${Namespace.XInclude.uri}"
         |>
         |  <para>Display TeX:<informalequation>
         |    <math mathjax:input="TeX" xmlns="${Namespace.MathML.uri}" xmlns:mathjax=
         |    "${MathJax.Namespace.uri}">
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
     s"""|${Namespace.Xml.header}
         |<article xml:id="test-id" xmlns="${Namespace.DocBook.uri}" version="${Namespace.DocBook.version}" xmlns:xi="${Namespace.XInclude.uri}">
         |  <para>Inline TeX:$$x = {-b \\pm \\sqrt{b^2-4ac} \\over 2a}.$$</para>
         |</article>
         |""".stripMargin
    ) shouldBe
     s"""|${Namespace.Xml.header}
         |<article version="${Namespace.DocBook.version}" xml:id="test-id" xmlns="${Namespace.DocBook.uri}" xmlns:xi="${Namespace.XInclude.uri}"
         |>
         |  <para>Inline TeX:<inlineequation>
         |    <math mathjax:input="inline-TeX" xmlns="${Namespace.MathML.uri}" xmlns:mathjax=
         |    "${MathJax.Namespace.uri}">
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
     s"""|${Namespace.Xml.header16}
         |<article xmlns="${Namespace.DocBook.uri}" version="${Namespace.DocBook.version}" xml:id="test-id" xmlns:xi="${Namespace.XInclude.uri}">
         |  <para>Explicit display TeX:<equation>$$$$x = {-b \\pm \\sqrt{b^2-4ac} \\over 2a}.$$$$</equation></para>
         |</article>""".stripMargin
    ) shouldBe
     s"""|${Namespace.Xml.header}
         |<article version="${Namespace.DocBook.version}" xml:id="test-id" xmlns="${Namespace.DocBook.uri}" xmlns:xi="${Namespace.XInclude.uri}"
         |>
         |  <para>Explicit display TeX:<equation>
         |    <math mathjax:input="TeX" xmlns="${Namespace.MathML.uri}" xmlns:mathjax=
         |    "${MathJax.Namespace.uri}">
         |      <mrow>
         |        <mi>x = {-b \\pm \\sqrt{b^2-4ac} \\over 2a}.</mi>
         |      </mrow>
         |    </math>
         |  </equation></para>
         |</article>
         |""".stripMargin
  }


  private val prettyPrinter: PrettyPrinter = new PrettyPrinter(alwaysStackElements =
    Set("article", "para", "equation", "informalequation", "inlineequation", "math", "mrow"))

  private def parse(string: String): String = {
    // Note: Saxon6 returns unmodifiable DOM that breaks toString() (which toString()?); using Saxon10.
    val node: Node = Saxon.Saxon10.parse(
      input = string,
      xmlReader = Xerces.getFilteredXMLReader(filters = Seq(
        new MathFilter(Configuration())
//        , new org.opentorah.xml.TracingFilter
      ))
    )
    val result = prettyPrinter.renderXml(node.asInstanceOf[Document].getDocumentElement)
//    println(result)
    result
  }
}
