package org.podval.archive19kislev.collector

import scala.xml.Elem

final class Page(val name: String, val isPresent: Boolean, val document: Document) {

  def displayName: String = Page.displayName(name)

  def pageReference: Elem = {
    val refClass = if (isPresent) "page" else "missing-page"
    <a class={refClass} href={s"documents/${document.name}.xml#${toXmlId(name)}"}>{displayName}</a>
  }

  private def toXmlId(name: String): String = "p" + name
}

object Page {
  private val numberOfDigitsInName = 3

  val frontSuffix = "-1"
  val backSuffix = "-2"
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
