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
    // Pages in Dubnov do not have -1/-2...
//    if (!name.endsWith(frontSuffix) && !name.endsWith(backSuffix))
//      throw new IllegalArgumentException(s"Invalid name $name")
//    checkBase(base(name))
    checkBase(name)
  }

  def checkBase(name: String): Unit = {
    if (name.takeWhile(_.isDigit).length != numberOfDigitsInName)
      throw new IllegalArgumentException()
    val s = base(name).drop(numberOfDigitsInName)
    if (!s.isEmpty && (s != "a"))
      throw new IllegalArgumentException(s"Illegal image name: $s [$name]")
  }

  private def base(name: String): String = name.dropRight(frontSuffix.length)

  def displayName(name: String): String =
    if (name.endsWith(frontSuffix)) name.dropRight(frontSuffix.length) else
    if (name.endsWith(backSuffix )) name.dropRight(backSuffix.length) + "об" else
      name
}
