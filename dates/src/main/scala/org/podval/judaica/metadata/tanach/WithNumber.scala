package org.podval.judaica.metadata.tanach

trait WithNumber {
  def n: Int
}

object WithNumber {
  def checkConsecutive(result: Seq[WithNumber], what: String): Seq[WithNumber] = {
    require(result.map(_.n) == (1 to result.length), s"Wrong $what numbers: $result")
    result
  }
}
