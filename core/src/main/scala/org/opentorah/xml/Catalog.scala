package org.opentorah.xml

object Catalog extends Dialect, Doctype:
  override val namespace: Namespace = Namespace(uri = "urn:oasis:names:tc:entity:xmlns:xml:catalog", prefix = null)

  override val mimeType: String = "application/xml"

  override val doctype: String = Doctype.string(
    rootElementName = "catalog",
    dtdId = "-//OASIS//DTD XML Catalogs V1.1//EN",
    dtdUri = "http://www.oasis-open.org/committees/entity/release/1.1/catalog.dtd"
  )

  override def prettyPrinter: PrettyPrinter = PrettyPrinter.default

  def catalog(content: ScalaXml.Nodes): ScalaXml.Element =
    <catalog xmlns={namespace.uri} prefer="public">{content}</catalog>

  def group(base: String, content: ScalaXml.Nodes): ScalaXml.Element =
    <group xml:base={base}>{content}</group>

  // There seems to be some confusion with the rewriteURI form:
  // Catalog DTD requires 'uriIdStartString' attribute (and that is what IntelliJ wants),
  // but XMLResolver looks for the 'uriStartString' attribute (and this seems to work in Oxygen).
  def rewriteUri(rewritePrefix: String, uriStartString: String): ScalaXml.Element =
    <rewriteURI rewritePrefix={rewritePrefix} uriStartString={uriStartString}/>

  def rewriteSystem(rewritePrefix: String, systemIdStartString: String): ScalaXml.Element =
    <rewriteSystem rewritePrefix={rewritePrefix} systemIdStartString={systemIdStartString}/>

  def public(publicId: String, uri: String): ScalaXml.Element =
    <public publicId={publicId} uri={uri}/>

  def nextCatalog(catalog: String): ScalaXml.Element =
    <nextCatalog catalog={catalog}/>

  def nextCatalogSystem: ScalaXml.Element =
    nextCatalog("/etc/xml/catalog")

  def dtd(substitutions: Map[String, String]): String = substitutions.toSeq.map(
    (name: String, value: String) => s"""<!ENTITY $name "$value">\n"""
  ).mkString
