package org.podval.docbook.gradle.mathjax

import org.podval.docbook.gradle.xml.Namespace

// Note: only MathJaxObj.getNormalNamespacePrefix() needs the prefix;
// everywhere else default mapping is assumed.
object MathML extends Namespace(uri = "http://www.w3.org/1998/Math/MathML", prefix = "mathml") {

  val mimeType: String = "application/mathml+xml"

  val math: String = "math"
  val mrow: String = "mrow"
  val mi: String = "mi"
}
