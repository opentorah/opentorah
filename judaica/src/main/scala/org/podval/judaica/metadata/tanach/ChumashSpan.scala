package org.podval.judaica.metadata.tanach

object ChumashSpan extends BookSpan {
  override type Book = Tanach.ChumashBook

  override protected def getBook(name: String): Tanach.ChumashBook = Tanach.getChumashForName(name)
}
