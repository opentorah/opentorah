package org.podval.docbook.gradle

import java.io.{Reader, StringReader}

import org.xml.sax.InputSource

final class EntitiesInputSource(entities: Map[String, String]) extends InputSource {

  override def getSystemId: String = null

  override def getCharacterStream: Reader = {
    println(prefix)
    new StringReader(prefix)
  }

  def prefix: String = {
    val lines: Seq[String] = entities.toSeq.map {
      case (name: String, value: String) => s"""<!ENTITY $name "$value">"""
    }

    lines.mkString("", "\n", "\n")
  }
}
