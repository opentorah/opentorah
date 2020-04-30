package org.opentorah.fop.xml

import java.io.File
import javax.xml.transform.{Source, URIResolver}
import org.slf4j.{Logger, LoggerFactory}
import org.xml.sax.{EntityResolver, InputSource}

final class Resolver(catalogFile: File) extends URIResolver with EntityResolver {

  private val logger: Logger = LoggerFactory.getLogger(classOf[Resolver])

  logger.info(s"Resolver(catalogFile = $catalogFile)")

  private val parentResolver: org.xmlresolver.Resolver = {
    val properties = new java.util.Properties
    properties.setProperty("cacheUnderHome", "yes")
    val configuration = new org.xmlresolver.Configuration(properties, null)
    val catalog = new org.xmlresolver.Catalog(configuration, catalogFile.getAbsolutePath)
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
      logger.error(s"$parameters\n  unresolved")
    } { result =>
      logger.debug(s"$parameters\n  resolved to: ${id(result)}")
    }

    result.getOrElse(null.asInstanceOf[R])
  }
}
