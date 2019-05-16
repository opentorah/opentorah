package org.podval.docbook.gradle

import org.scalatest.{FlatSpec, Matchers}
import org.podval.docbook.gradle.mathjax.MathReader

class MathReaderTest extends FlatSpec with Matchers {

  "MathReader" should "work for wrapped display TeX" in {
    // Serializer outputs UTF-16; xml namespace is made explicit; order of attributes and spacing are different -
    // but other than that, the document is the same.
    parse(
      """|<?xml version="1.0" encoding="UTF-16"?>
         |<article xmlns="http://docbook.org/ns/docbook" version="5.0" xml:id="test-id"
         |         xmlns:xi="http://www.w3.org/2001/XInclude">
         |  <para>
         |    Wrapped display TeX:<informalequation>
         |    <math xmlns="http://www.w3.org/1998/Math/MathML"
         |          xmlns:mathjax="http://podval.org/mathjax/ns/ext" mathjax:mode="TeX">
         |      <mrow><mi>x = {-b \pm \sqrt{b^2-4ac} \over 2a}.</mi></mrow>
         |    </math></informalequation>
         |  </para>
         |</article>""".stripMargin
    ) shouldBe
      """|<?xml version="1.0" encoding="UTF-16"?><article xmlns="http://docbook.org/ns/docbook" version="5.0" xmlns:xml="http://www.w3.org/XML/1998/namespace" xml:id="test-id" xmlns:xi="http://www.w3.org/2001/XInclude">
         |  <para>
         |    Wrapped display TeX:<informalequation>
         |    <math xmlns="http://www.w3.org/1998/Math/MathML" mathjax:mode="TeX" xmlns:mathjax="http://podval.org/mathjax/ns/ext">
         |      <mrow>
         |               <mi>x = {-b \pm \sqrt{b^2-4ac} \over 2a}.</mi>
         |            </mrow>
         |    </math>
         |      </informalequation>
         |  </para>
         |</article>
         |""".stripMargin
  }

  it should "work for display TeX" in {
    parse(
      """|<?xml version="1.0" encoding="UTF-8"?>
         |<article xml:id="test-id"
         |         xmlns="http://docbook.org/ns/docbook" version="5.0"
         |         xmlns:xi="http://www.w3.org/2001/XInclude">
         |  <para>Display TeX:$$x = {-b \pm \sqrt{b^2-4ac} \over 2a}.$$</para>
         |</article>""".stripMargin
    ) shouldBe
      """|<?xml version="1.0" encoding="UTF-16"?><article xmlns="http://docbook.org/ns/docbook" version="5.0" xmlns:xml="http://www.w3.org/XML/1998/namespace" xml:id="test-id" xmlns:xi="http://www.w3.org/2001/XInclude">
         |  <para>Display TeX:<informalequation>
         |         <math xmlns="http://www.w3.org/1998/Math/MathML" mathjax:mode="TeX" xmlns:mathjax="http://podval.org/mathjax/ns/ext">
         |            <mrow>
         |               <mi>x = {-b \pm \sqrt{b^2-4ac} \over 2a}.</mi>
         |            </mrow>
         |         </math>
         |      </informalequation>
         |   </para>
         |</article>
         |""".stripMargin
  }

  it should "work for inline TeX" in {
    parse(
      """|<?xml version="1.0" encoding="UTF-8"?>
         |<article xml:id="test-id"
         |         xmlns="http://docbook.org/ns/docbook" version="5.0"
         |         xmlns:xi="http://www.w3.org/2001/XInclude">
         |  <para>Inline TeX:$x = {-b \pm \sqrt{b^2-4ac} \over 2a}.$</para>
         |</article>
         |""".stripMargin
    ) shouldBe
      """|<?xml version="1.0" encoding="UTF-16"?><article xmlns="http://docbook.org/ns/docbook" version="5.0" xmlns:xml="http://www.w3.org/XML/1998/namespace" xml:id="test-id" xmlns:xi="http://www.w3.org/2001/XInclude">
         |  <para>Inline TeX:<inlineequation>
         |         <math xmlns="http://www.w3.org/1998/Math/MathML" mathjax:mode="TeX" xmlns:mathjax="http://podval.org/mathjax/ns/ext">
         |            <mrow>
         |               <mi>x = {-b \pm \sqrt{b^2-4ac} \over 2a}.</mi>
         |            </mrow>
         |         </math>
         |      </inlineequation>
         |   </para>
         |</article>
         |""".stripMargin
  }

  it should "work for equation display TeX" in {
    parse(
      """|<?xml version="1.0" encoding="UTF-16"?>
         |<article xmlns="http://docbook.org/ns/docbook" version="5.0" xml:id="test-id"
         |         xmlns:xi="http://www.w3.org/2001/XInclude">
         |  <para>Explicit display TeX:<equation>$$x = {-b \\pm \\sqrt{b^2-4ac} \\over 2a}.$$</equation></para>
         |</article>""".stripMargin
    ) shouldBe
      """|<?xml version="1.0" encoding="UTF-16"?><article xmlns="http://docbook.org/ns/docbook" version="5.0" xmlns:xml="http://www.w3.org/XML/1998/namespace" xml:id="test-id" xmlns:xi="http://www.w3.org/2001/XInclude">
         |  <para>Explicit display TeX:<equation>
         |         <math xmlns="http://www.w3.org/1998/Math/MathML" mathjax:mode="TeX" xmlns:mathjax="http://podval.org/mathjax/ns/ext">
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
//    """|<?xml version="1.0" encoding="UTF-8"?>
//       |<article xml:id="test-id"
//       |         xmlns="http://docbook.org/ns/docbook" version="5.0"
//       |         xmlns:xi="http://www.w3.org/2001/XInclude">
//       |  <para>
//       |    Wrapped display TeX:<inlineequation>
//       |    <math xmlns="http://www.w3.org/1998/Math/MathML"
//       |          xmlns:mathjax="http://podval.org/mathjax/ns/ext" mathjax:mode="TeX">
//       |      <mrow><mi>x = {-b \pm \sqrt{b^2-4ac} \over 2a}.</mi></mrow>
//       |    </math></inlineequation>
//       |  </para>
//       |</article>""".stripMargin

  private def parse(string: String): String = {
    val mathReader = new MathReader
    mathReader.setParent(Xml.getFilteredXMLReader(filters = Seq.empty))
    val result = Xml.parse(string, mathReader, new TestLogger)
    Xml.toString(result)
  }
}
