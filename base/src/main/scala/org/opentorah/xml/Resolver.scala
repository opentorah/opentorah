package org.opentorah.xml

import java.io.File
import javax.xml.transform.{Source, URIResolver}
import org.xml.sax.{EntityResolver, InputSource}

final class Resolver(catalogFile: File) extends URIResolver with EntityResolver {

  xmlLogger.info(s"Resolver(catalogFile = $catalogFile)")

  private val parentResolver: org.xmlresolver.Resolver = {
    val configuration: org.xmlresolver.XMLResolverConfiguration = new org.xmlresolver.XMLResolverConfiguration()
    configuration.setFeature[java.lang.Boolean](org.xmlresolver.ResolverFeature.CACHE_UNDER_HOME, true)
    new org.xmlresolver.Resolver(new org.xmlresolver.Catalog(configuration, catalogFile.getAbsolutePath))
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
