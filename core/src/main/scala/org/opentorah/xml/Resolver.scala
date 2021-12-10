package org.opentorah.xml

import org.slf4j.Logger
import org.xml.sax.{EntityResolver, InputSource}
import java.io.File
import javax.xml.transform.{Source, URIResolver}
import scala.jdk.CollectionConverters.SeqHasAsJava

final class Resolver(catalogFile: File, logger: Logger) extends URIResolver, EntityResolver:

  logger.info(s"Resolver(catalogFile = $catalogFile)")

  private val parentResolver: org.xmlresolver.Resolver =
    val configuration: org.xmlresolver.XMLResolverConfiguration = org.xmlresolver.XMLResolverConfiguration()

    configuration.setFeature[java.util.List[String]](org.xmlresolver.ResolverFeature.CATALOG_FILES,
      List[String](catalogFile.getAbsolutePath).asJava)

    // To disable cache:
    //configuration.setFeature[java.lang.Boolean](org.xmlresolver.ResolverFeature.CACHE_UNDER_HOME,  false)

    // To enable validation:
    //configuration.setFeature[java.lang.String](org.xmlresolver.ResolverFeature.CATALOG_LOADER_CLASS, "org.xmlresolver.loaders.ValidatingXmlLoader")

    org.xmlresolver.Resolver(configuration)

  override def resolve(href: String, base: String): Source = resolve[Source](
    call = _.resolve(href, base),
    parameters = s"Resolver.resolve(href=$href, base=$base)",
    id = _.getSystemId,
    // `file:`-based URIs are not resolved: calling parser will arrive at the same result.
    ignoreUnresolved = base.startsWith("file:")
  )

  override def resolveEntity(publicId: String, systemId: String): InputSource = resolve[InputSource](
    call = _.resolveEntity(publicId, systemId),
    parameters = s"Resolver.resolveEntity(publicId=$publicId, systemId=$systemId)",
    id = _.getSystemId,
    ignoreUnresolved = systemId.startsWith("file:")
  )

  private def resolve[R](
    call: org.xmlresolver.Resolver => R,
    parameters: String,
    id: R => String,
    ignoreUnresolved: Boolean
  ): R =
    val result = Option(call(parentResolver))

    result.fold(
      if ignoreUnresolved then
        logger.debug(s"$parameters\n  unresolved")
      else
        logger.error(s"$parameters\n  unresolved")
    )(result =>
      logger.debug(s"$parameters\n  resolved to: ${id(result)}")
    )

    result.getOrElse(null.asInstanceOf[R])
