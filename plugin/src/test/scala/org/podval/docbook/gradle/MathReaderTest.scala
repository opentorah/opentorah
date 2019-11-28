package org.podval.docbook.gradle

import org.podval.docbook.gradle.mathjax.{Configuration, MathJax, MathML, MathReader}
import org.podval.docbook.gradle.plugin.DocBook
import org.podval.docbook.gradle.xml.{Namespace, Xml}
import org.scalatest.{FlatSpec, Matchers}

class MathReaderTest extends FlatSpec with Matchers {

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
    var logger = new TestLogger
    val result = Xml.parse(
      input = string,
      xmlReader = Xml.getFilteredXMLReader(filters = Seq(
        new MathReader(Configuration(), logger)
        /* , new TracingFilter */
      )),
      logger
    )
    Xml.toString(result)
  }
}
