package org.digitaljudaica.store

import java.io.File

import org.digitaljudaica.store.metadata.Xml

import scala.xml.Elem

class Store {

}

object Store {
  def main(args: Array[String]): Unit = {
    val xml: Elem = Xml.load(new File("docs").getAbsoluteFile, "store")
    println(Xml.format(xml))
  }
}
