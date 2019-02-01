package org.podval.docbook.gradle

trait Evaluator {
  def eval(expression: String): Option[AnyRef]
}
