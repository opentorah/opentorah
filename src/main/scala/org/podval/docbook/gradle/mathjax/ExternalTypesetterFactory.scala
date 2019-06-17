package org.podval.docbook.gradle.mathjax

import org.podval.docbook.gradle.node.Installation
import org.podval.docbook.gradle.util.Logger

object ExternalTypesetterFactory extends Typesetter.Factory {
  override def get(installation: Installation, configuration: Configuration, logger: Logger): Typesetter =
    new Typesetter(configuration, logger) {
      override def typeset(optionsMap: Map[String, Any], outputName: String): String = {
        ???
      }
    }
}
