package org.podval.judaica.metadata.tanach

object ProphetSpan extends BookSpan {
  override type Book = Tanach.ProphetsBook

  override protected def getBook(name: String): Tanach.ProphetsBook = Tanach.getProhetForName(name)
}
