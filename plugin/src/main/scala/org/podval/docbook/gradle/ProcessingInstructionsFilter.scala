package org.podval.docbook.gradle

import org.xml.sax.XMLReader
import org.xml.sax.helpers.XMLFilterImpl

class ProcessingInstructionsFilter(
  parent: XMLReader,
  substitutions: Map[String, String],
  logger: Logger) extends XMLFilterImpl(parent)
{
  override def processingInstruction(target: String, data: String): Unit = {
    logger.info(s"ProcessingInstructionsFilter.processingInstruction(target = $target, data = [$data])")
    if (target == "eval") {
      val expression: String = data.trim
      val result: String = substitutions.getOrElse(expression, s"Evaluation failed for [$expression]")

      val characters: Array[Char] = result.toCharArray
      this.characters(characters, 0, result.length)
    } else {
      super.processingInstruction(target, data)
    }
  }
}
