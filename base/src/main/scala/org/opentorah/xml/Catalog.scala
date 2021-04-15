package org.opentorah.xml

object Catalog extends Dialect with Doctype {
  override val namespace: Namespace = Namespace(uri = "urn:oasis:names:tc:entity:xmlns:xml:catalog", prefix = null)

  override val mimeType: String = "application/xml"

  val dtdId: String = "-//OASIS//DTD XML Catalogs V1.1//EN"

  val dtdUri: String = "http://www.oasis-open.org/committees/entity/release/1.1/catalog.dtd"

  override val doctype: String = s"""<!DOCTYPE catalog PUBLIC "$dtdId" "$dtdUri">"""

  def catalog(content: Xml.Nodes): Xml.Element =
    <catalog xmlns={namespace.uri} prefer="public">{content}</catalog>

  def group(base: String, content: Xml.Nodes): Xml.Element =
    <group xml:base={base}>{content}</group>

  // There seems to be some confusion with the rewriteURI form:
  // Catalog DTD requires 'uriIdStartString' attribute (and that is what IntelliJ wants),
  // but XMLResolver looks for the 'uriStartString' attribute (and this seems to work in Oxygen).
  def rewriteUri(rewritePrefix: String, uriStartString: String): Xml.Element =
    <rewriteURI rewritePrefix={rewritePrefix} uriStartString={uriStartString}/>

  def rewriteSystem(rewritePrefix: String, systemIdStartString: String): Xml.Element =
    <rewriteSystem rewritePrefix={rewritePrefix} systemIdStartString={systemIdStartString}/>

  def public(publicId: String, uri: String): Xml.Element =
    <public publicId={publicId} uri={uri}/>

  def nextCatalog(catalog: String): Xml.Element =
    <nextCatalog catalog={catalog}/>

  def nextCatalogSystem: Xml.Element =
    nextCatalog("/etc/xml/catalog")

  def dtd(substitutions: Map[String, String]): String = substitutions.toSeq.map {
    case (name: String, value: String) => s"""<!ENTITY $name "$value">\n"""
  }.mkString
}
