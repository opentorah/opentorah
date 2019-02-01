package org.podval.docbook.gradle

import java.text.DateFormat
import java.util.Date

import org.xml.sax.XMLReader
import org.xml.sax.helpers.XMLFilterImpl

class ProcessingInstructionsFilter(
  parent: XMLReader,
  evaluator: Evaluator,
  logger: Logger) extends XMLFilterImpl(parent)
{
  override def processingInstruction(target: String, data: String): Unit = {
    logger.info(s"ProcessingInstructionsFilter.processingInstruction(target = $target, data = [$data])")
    if (target == "eval") {
      val expression: String = data.trim
      val value: Option[String] = eval(expression).map(_.toString)
      val result: String = value.getOrElse(s"Evaluation failed for [$expression]")

      val characters: Array[Char] = result.toCharArray
      this.characters(characters, 0, result.length)
    } else {
      super.processingInstruction(target, data)
    }
  }

  private def eval(expression: String): Option[AnyRef] = {
    if (expression == "date") Some(DateFormat.getDateInstance(DateFormat.LONG).format(new Date)) else
      evaluator.eval(expression)
  }
}
