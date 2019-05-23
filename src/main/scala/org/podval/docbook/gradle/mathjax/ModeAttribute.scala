package org.podval.docbook.gradle.mathjax

import org.podval.docbook.gradle.xml.{Attribute, Namespace}

/**
  * Type of the input: TeX, MathML, AsciiMath.
  */
@SerialVersionUID(1L)
case object ModeAttribute extends Attribute[MathJax.Input] {
  override def namespace: Namespace = MathJaxNamespace
  override def name: String = "mode"

  override def toString(value: MathJax.Input): String = value.input

  override def fromString(value: String): MathJax.Input = MathJax.inputForName(value)

  override def default: MathJax.Input = MathJax.MathML
}
