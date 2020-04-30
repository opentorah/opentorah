package org.opentorah.docbook.plugin

import org.opentorah.fop.xml.WarningFilter
import org.slf4j.{Logger, LoggerFactory}

final class EvalFilter(
  substitutions: Map[String, String]
) extends WarningFilter {

  private val logger: Logger = LoggerFactory.getLogger(classOf[EvalFilter])

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
