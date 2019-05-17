package org.podval.docbook.gradle.mathjax

import org.apache.fop.fo.{FONode, XMLObj}
import org.podval.docbook.gradle.Namespace.MathML

class MathJaxObj(parent: FONode) extends XMLObj(parent) {

  override def getNamespaceURI: String = MathML.uri

  override def getNormalNamespacePrefix: String = MathML.prefix
}
