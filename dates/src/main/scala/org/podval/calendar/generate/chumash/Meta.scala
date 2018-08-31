package org.podval.calendar.generate.chumash

import java.net.URL

import scala.xml.{Elem, Utility, XML}

object Meta {
  def getURL(name: String): URL = getClass.getClassLoader.getResource(name)

  def childURL(parent: URL, name: String): URL = new URL(
    parent.getProtocol, parent.getHost, parent.getPort, parent.getFile + "/" + name, null);

  def childFileURL(parent: URL, name: String): URL = new URL(
    parent.getProtocol, parent.getHost, parent.getPort, parent.getFile + "/" + name + ".xml", null);

  def loadResource(url: URL): Elem = {
    val result = XML.load(url.openStream())
    Utility.trimProper(result).asInstanceOf[Elem]
  }

  def loadResource(name: String): Elem = {
    val is = getClass.getClassLoader.getResourceAsStream(name + ".xml")
    val result = XML.load(is)
    Utility.trimProper(result).asInstanceOf[Elem]
  }


  def main(args: Array[String]): Unit = {
    val base: URL = getURL("Tanach")
    val tanachURL: URL = childFileURL(base, "Tanach")
    val tanach = loadResource(tanachURL)

    val label = tanach.label
    val z = 0
  }
}
