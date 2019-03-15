package org.podval.archive19kislev.collector

final case class Page(name: String, isPresent: Boolean, document: Document) {
  def displayName: String = Page.displayName(name)
}

object Page {
  private val numberOfDigitsInName = 3

  private val frontSuffix = "-1"
  private val backSuffix = "-2"
  require(frontSuffix.length == backSuffix.length)

  def check(name: String): Unit = {
    require(name.endsWith(frontSuffix) || name.endsWith(backSuffix), s"Invalid name $name")
    checkBase(base(name))
  }

  def checkBase(name: String): Unit = {
    require(name.takeWhile(_.isDigit).length == numberOfDigitsInName)
    val s = base(name).drop(numberOfDigitsInName)
    require(s.isEmpty || (s == "a"))
  }

  private def base(name: String): String = name.dropRight(frontSuffix.length)

  def displayName(name: String): String =
    if (name.endsWith(frontSuffix)) name.dropRight(frontSuffix.length) else
    if (name.endsWith(backSuffix )) name.dropRight(backSuffix.length) + "об" else
      name
}
