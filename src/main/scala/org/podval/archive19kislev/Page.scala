package org.podval.archive19kislev

import scala.xml.Elem


final class Page(val name: String, val isPresent: Boolean, val document: Document) {

  def displayName: String = Name.display(name)


  def pageReference: Elem = {
    val refClass = if (isPresent) "page" else "missing-page"
    <a class={refClass} href={s"documents/${document.name}.xml#${Name.toXmlId(name)}"}>{displayName}</a>
  }
}
