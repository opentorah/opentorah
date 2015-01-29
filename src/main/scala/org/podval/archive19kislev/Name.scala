package org.podval.archive19kislev


object Name {

  private[this] val numberOfDigitsInName = 3

  private[this] val frontSuffix = "-1"
  private[this] val backSuffix = "-2"

  require(frontSuffix.length == backSuffix.length)


  private def isFront(name: String): Boolean = name.endsWith(frontSuffix)
  private def isBack(name: String): Boolean = name.endsWith(backSuffix)
  private def base(name: String): String = name.dropRight(frontSuffix.length)


  def check(name: String): Unit = {
    require(isFront(name) || isBack(name), s"Invalid name $name")
    checkBase(base(name))
  }


  def checkBase(name: String): Unit = {
    require(name.takeWhile(_.isDigit).length == numberOfDigitsInName)
    val s = base(name).drop(numberOfDigitsInName)
    require(s.isEmpty || (s == "a"))
  }


  def display(n: String): String = {
    if (n.endsWith(frontSuffix)) n.dropRight(frontSuffix.length) else
    if (n.endsWith(backSuffix )) n.dropRight(backSuffix.length) + "об" else
      n
  }


  def toXmlId(name: String): String = "p" + name
}
