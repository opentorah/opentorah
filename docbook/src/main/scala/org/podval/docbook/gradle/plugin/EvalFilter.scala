package org.podval.docbook.gradle.plugin

import org.opentorah.fop.util.Logger
import org.opentorah.fop.xml.WarningFilter

final class EvalFilter(
  substitutions: Map[String, String],
  logger: Logger
) extends WarningFilter {

  override def processingInstruction(target: String, data: String): Unit = {
    logger.debug(s"EvalFilter.processingInstruction(target = $target, data = [$data])")

    if (target == "eval") {
      val expression: String = data.trim
      val result: String = substitutions.getOrElse(expression, {
        val message = s"Evaluation failed for [$expression]"
        warning(message)
        message
      })

      val characters: Array[Char] = result.toCharArray
      this.characters(characters, 0, result.length)
    } else {
      super.processingInstruction(target, data)
    }
  }
}
