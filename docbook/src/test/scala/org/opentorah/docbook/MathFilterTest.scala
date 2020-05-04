package org.opentorah.docbook

import org.opentorah.docbook.plugin.{DocBook, MathFilter}
import org.opentorah.mathjax.{Configuration, MathJax, MathML}
import org.opentorah.xml.{Namespace, Saxon, Xerces, Xml}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.w3c.dom.Node

class MathFilterTest extends AnyFlatSpecLike with Matchers {

  it should "work for wrapped display TeX" in {
    // Serializer outputs UTF-16; xml namespace is made explicit; order of attributes and spacing are different -
    // but other than that, the document is the same.
    parse(
     s"""|${Xml.header16}
         |<article ${DocBook.Namespace.withVersion} xml:id="test-id" ${Namespace.XInclude}>
         |  <para>
         |    Wrapped display TeX:<informalequation>
         |    <math ${MathML.Namespace.default}
         |          ${MathJax.Namespace} mathjax:input="TeX">
         |      <mrow><mi>x = {-b \\pm \\sqrt{b^2-4ac} \\over 2a}.</mi></mrow>
         |    </math></informalequation>
         |  </para>
         |</article>""".stripMargin
    ) shouldBe
     s"""|${Xml.header16}<article ${DocBook.Namespace.withVersion} ${Namespace.Xml} xml:id="test-id" ${Namespace.XInclude}>
         |  <para>
         |    Wrapped display TeX:<informalequation>
         |    <math ${MathML.Namespace.default} display="block" mathjax:input="TeX" ${MathJax.Namespace}>
         |      <mrow>
         |               <mi>x = {-b \\pm \\sqrt{b^2-4ac} \\over 2a}.</mi>
         |            </mrow>
         |    </math>
         |      </informalequation>
         |  </para>
         |</article>
         |""".stripMargin
  }

  it should "work for display TeX" in {
    parse(
     s"""|${Xml.header}
         |<article xml:id="test-id" ${DocBook.Namespace.withVersion} ${Namespace.XInclude}>
         |  <para>Display TeX:$$$$x = {-b \\pm \\sqrt{b^2-4ac} \\over 2a}.$$$$</para>
         |</article>""".stripMargin
    ) shouldBe
     s"""|${Xml.header16}<article ${DocBook.Namespace.withVersion} ${Namespace.Xml} xml:id="test-id" ${Namespace.XInclude}>
         |  <para>Display TeX:<informalequation>
         |         <math ${MathML.Namespace.default} mathjax:input="TeX" ${MathJax.Namespace}>
         |            <mrow>
         |               <mi>x = {-b \\pm \\sqrt{b^2-4ac} \\over 2a}.</mi>
         |            </mrow>
         |         </math>
         |      </informalequation>
         |   </para>
         |</article>
         |""".stripMargin
  }

  it should "work for inline TeX" in {
    parse(
     s"""|${Xml.header}
         |<article xml:id="test-id" ${DocBook.Namespace.withVersion} ${Namespace.XInclude}>
         |  <para>Inline TeX:$$x = {-b \\pm \\sqrt{b^2-4ac} \\over 2a}.$$</para>
         |</article>
         |""".stripMargin
    ) shouldBe
     s"""|${Xml.header16}<article ${DocBook.Namespace.withVersion} ${Namespace.Xml} xml:id="test-id" ${Namespace.XInclude}>
         |  <para>Inline TeX:<inlineequation>
         |         <math ${MathML.Namespace.default} mathjax:input="inline-TeX" ${MathJax.Namespace}>
         |            <mrow>
         |               <mi>x = {-b \\pm \\sqrt{b^2-4ac} \\over 2a}.</mi>
         |            </mrow>
         |         </math>
         |      </inlineequation>
         |   </para>
         |</article>
         |""".stripMargin
  }

  it should "work for equation display TeX" in {
    parse(
     s"""|${Xml.header16}
         |<article ${DocBook.Namespace.withVersion} xml:id="test-id" ${Namespace.XInclude}>
         |  <para>Explicit display TeX:<equation>$$$$x = {-b \\pm \\sqrt{b^2-4ac} \\over 2a}.$$$$</equation></para>
         |</article>""".stripMargin
    ) shouldBe
     s"""|${Xml.header16}<article ${DocBook.Namespace.withVersion} ${Namespace.Xml} xml:id="test-id" ${Namespace.XInclude}>
         |  <para>Explicit display TeX:<equation>
         |         <math ${MathML.Namespace.default} mathjax:input="TeX" ${MathJax.Namespace}>
         |            <mrow>
         |               <mi>x = {-b \\pm \\sqrt{b^2-4ac} \\over 2a}.</mi>
         |            </mrow>
         |         </math>
         |      </equation>
         |   </para>
         |</article>
         |""".stripMargin
  }

//  val wrappedInlineTex: String =
//   s"""|${Xml.header}
//       |<article xml:id="test-id" ${DocBook.withVersion} ${Namespace.XInclude}>
//       |  <para>
//       |    Wrapped display TeX:<inlineequation>
//       |    <math ${MathML.default} ${MathJaxNamespace } mathjax:input="TeX">
//       |      <mrow><mi>x = {-b \pm \sqrt{b^2-4ac} \over 2a}.</mi></mrow>
//       |    </math></inlineequation>
//       |  </para>
//       |</article>""".stripMargin

  private def parse(string: String): String = {
    // Saxon 6 returns unmodifiable DOM that breaks toString(); using Saxon 9.
    val result: Node = Saxon.Saxon9.parse(
      input = string,
      xmlReader = Xerces.getFilteredXMLReader(filters = Seq(
        new MathFilter(Configuration())
        /* , new TracingFilter */
      ))
    )
    Xerces.toString(result)
  }
}
