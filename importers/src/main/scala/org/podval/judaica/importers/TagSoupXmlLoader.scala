package org.podval.judaica.importers

import scala.xml.{Elem, XML}
import scala.xml.factory.XMLLoader
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl

object TagSoupXmlLoader {
    private val factory = new SAXFactoryImpl()

    def get(): XMLLoader[Elem] = {
        XML.withSAXParser(factory.newSAXParser())
    }
}
