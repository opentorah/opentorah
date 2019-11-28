package org.podval.docbook.gradle.xml

import org.xml.sax.helpers.XMLFilterImpl
import org.xml.sax.{Locator, SAXParseException}

trait WarningFilter extends XMLFilterImpl {
  private var locator: Option[Locator] = None

  protected def warning(message: String): Unit = getErrorHandler.warning(new SAXParseException(message, locator.orNull))

  override def setDocumentLocator(locator: Locator): Unit = {
    super.setDocumentLocator(locator)
    this.locator = Some(locator)
  }
}
