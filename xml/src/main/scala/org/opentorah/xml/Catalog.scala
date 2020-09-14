package org.opentorah.xml

object Catalog extends Dialect {
  override val namespace: Namespace = Namespace(uri = "urn:oasis:names:tc:entity:xmlns:xml:catalog", prefix = null)

  override val mimeType: String = "application/xml"

  val dtdId: String = "-//OASIS//DTD XML Catalogs V1.1//EN"

  val dtdUri: String = "http://www.oasis-open.org/committees/entity/release/1.1/catalog.dtd"

  val doctype: String = s"""<!DOCTYPE catalog PUBLIC "$dtdId" "$dtdUri">"""
}
