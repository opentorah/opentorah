package org.opentorah.xml

import java.io.File
import javax.xml.transform.{Source, URIResolver}
import org.xml.sax.{EntityResolver, InputSource}

final class Resolver(catalogFile: File) extends URIResolver with EntityResolver {

  xmlLogger.info(s"Resolver(catalogFile = $catalogFile)")

  private val parentResolver: org.xmlresolver.Resolver = {
    val properties: java.util.Properties = new java.util.Properties
    properties.setProperty("cacheUnderHome", "yes")
    val configuration: org.xmlresolver.Configuration = new org.xmlresolver.Configuration(properties, null)
    val catalog: org.xmlresolver.Catalog = new org.xmlresolver.Catalog(configuration, catalogFile.getAbsolutePath)
    new org.xmlresolver.Resolver(catalog)
  }

  override def resolve(href: String, base: String): Source = resolve[Source](
    call = _.resolve(href, base),
    parameters = s"Resolver.resolve(href=$href, base=$base)",
    id = _.getSystemId
  )

  override def resolveEntity(publicId: String, systemId: String): InputSource = resolve[InputSource](
    call = _.resolveEntity(publicId, systemId),
    parameters = s"Resolver.resolveEntity(publicId=$publicId, systemId=$systemId)",
    id = _.getSystemId
  )

  private def resolve[R](
    call: org.xmlresolver.Resolver => R,
    parameters: String,
    id: R => String
  ): R = {
    val result = Option(call(parentResolver))

    result.fold {
      xmlLogger.error(s"$parameters\n  unresolved")
    } { result =>
      xmlLogger.debug(s"$parameters\n  resolved to: ${id(result)}")
    }

    result.getOrElse(null.asInstanceOf[R])
  }
}
