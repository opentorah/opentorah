package org.podval.docbook.gradle.mathjax

import org.podval.docbook.gradle.xml.{Attribute, Namespace}

// Typesetting mode: TeX, AsciiMath, MathML.
@SerialVersionUID(1L)
case object ModeAttribute extends Attribute.StringAttribute {
  override def namespace: Namespace = MathJaxNamespace
  override def name: String = "mode"
}
