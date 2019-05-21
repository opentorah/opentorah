package org.podval.docbook.gradle.mathjax

import org.podval.docbook.gradle.xml.{Attribute, Namespace}

// Display mode: inline or block (display math).
@SerialVersionUID(1L)
case object DisplayAttribute extends Attribute[Boolean] {
  private val inline: String = "inline"
  private val block: String = "block"
  private val values: Set[String] = Set(inline, block)

  override def namespace: Namespace = MathML
  override def name: String = "display"

  override def fromString(value: String): Boolean = {
    require(values.contains(value))
    value == inline
  }

  override def toString(value: Boolean): String = if (value) inline else block
}
