package org.podval.docbook.gradle.xml

import org.podval.fop.util.Logger

final class ProcessingInstructionsFilter(
  substitutions: Map[String, String],
  logger: Logger
) extends WarningFilter {

  override def processingInstruction(target: String, data: String): Unit = {
    logger.debug(s"ProcessingInstructionsFilter.processingInstruction(target = $target, data = [$data])")

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
