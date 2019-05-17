package org.podval.docbook.gradle

import org.scalatest.{FlatSpec, Matchers}
import org.podval.docbook.gradle.mathjax.MathReader

class MathReaderTest extends FlatSpec with Matchers {

  "MathReader" should "work for wrapped display TeX" in {
    // Serializer outputs UTF-16; xml namespace is made explicit; order of attributes and spacing are different -
    // but other than that, the document is the same.
    parse(
     s"""|<?xml version="1.0" encoding="UTF-16"?>
         |<article ${Namespace.DocBook} version="5.0" xml:id="test-id"
         |         ${Namespace.XInclude}>
         |  <para>
         |    Wrapped display TeX:<informalequation>
         |    <math ${Namespace.MathML.default}
         |          ${Namespace.MathJax} mathjax:mode="TeX">
         |      <mrow><mi>x = {-b \\pm \\sqrt{b^2-4ac} \\over 2a}.</mi></mrow>
         |    </math></informalequation>
         |  </para>
         |</article>""".stripMargin
    ) shouldBe
     s"""|<?xml version="1.0" encoding="UTF-16"?><article ${Namespace.DocBook} version="5.0" ${Namespace.Xml} xml:id="test-id" ${Namespace.XInclude}>
         |  <para>
         |    Wrapped display TeX:<informalequation>
         |    <math ${Namespace.MathML.default} display="block" mathjax:mode="TeX" ${Namespace.MathJax}>
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
     s"""|<?xml version="1.0" encoding="UTF-8"?>
         |<article xml:id="test-id"
         |         ${Namespace.DocBook} version="5.0"
         |         ${Namespace.XInclude}>
         |  <para>Display TeX:$$$$x = {-b \\pm \\sqrt{b^2-4ac} \\over 2a}.$$$$</para>
         |</article>""".stripMargin
    ) shouldBe
     s"""|<?xml version="1.0" encoding="UTF-16"?><article ${Namespace.DocBook} version="5.0" ${Namespace.Xml} xml:id="test-id" ${Namespace.XInclude}>
         |  <para>Display TeX:<informalequation>
         |         <math ${Namespace.MathML.default} display="block" mathjax:mode="TeX" ${Namespace.MathJax}>
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
     s"""|<?xml version="1.0" encoding="UTF-8"?>
         |<article xml:id="test-id"
         |         ${Namespace.DocBook} version="5.0"
         |         ${Namespace.XInclude}>
         |  <para>Inline TeX:$$x = {-b \\pm \\sqrt{b^2-4ac} \\over 2a}.$$</para>
         |</article>
         |""".stripMargin
    ) shouldBe
     s"""|<?xml version="1.0" encoding="UTF-16"?><article ${Namespace.DocBook} version="5.0" ${Namespace.Xml} xml:id="test-id" ${Namespace.XInclude}>
         |  <para>Inline TeX:<inlineequation>
         |         <math ${Namespace.MathML.default} display="inline" mathjax:mode="TeX" ${Namespace.MathJax}>
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
     s"""|<?xml version="1.0" encoding="UTF-16"?>
         |<article ${Namespace.DocBook} version="5.0" xml:id="test-id"
         |         ${Namespace.XInclude}>
         |  <para>Explicit display TeX:<equation>$$$$x = {-b \\pm \\sqrt{b^2-4ac} \\over 2a}.$$$$</equation></para>
         |</article>""".stripMargin
    ) shouldBe
     s"""|<?xml version="1.0" encoding="UTF-16"?><article ${Namespace.DocBook} version="5.0" ${Namespace.Xml} xml:id="test-id" ${Namespace.XInclude}>
         |  <para>Explicit display TeX:<equation>
         |         <math ${Namespace.MathML.default} display="block" mathjax:mode="TeX" ${Namespace.MathJax}>
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
//   s"""|<?xml version="1.0" encoding="UTF-8"?>
//       |<article xml:id="test-id"
//       |         ${Namespace.DocBook} version="5.0"
//       |         ${Namespace.XInclude}>
//       |  <para>
//       |    Wrapped display TeX:<inlineequation>
//       |    <math ${Namespace.MathML.default}
//       |          ${Namespace.MathJax} mathjax:mode="TeX">
//       |      <mrow><mi>x = {-b \pm \sqrt{b^2-4ac} \over 2a}.</mi></mrow>
//       |    </math></inlineequation>
//       |  </para>
//       |</article>""".stripMargin

  // TODO a few more tests for the display mode functionality?

  private def parse(string: String): String = {
    val result = Xml.parse(
      input = string,
      xmlReader = Xml.getFilteredXMLReader(filters = Seq(new MathReader /* , new TracingFilter */)),
      logger = new TestLogger
    )
    Xml.toString(result)
  }
}
