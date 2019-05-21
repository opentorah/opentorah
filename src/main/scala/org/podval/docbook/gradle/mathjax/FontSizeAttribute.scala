package org.podval.docbook.gradle.mathjax

import org.podval.docbook.gradle.xml.{Attribute, Namespace}

// Font size used for the output (mathsize).
@SerialVersionUID(1L)
case object FontSizeAttribute extends Attribute.FloatAttribute {
  override def namespace: Namespace = MathJaxNamespace
  override def name: String = "fontSize"
}
