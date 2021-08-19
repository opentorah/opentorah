package org.opentorah.store

import org.opentorah.metadata.{Language, WithNames}
import org.opentorah.util.Files
import org.opentorah.xml.{Elements, Parser, PrettyPrinter, ScalaXml}
import zio.ZIO

trait Store extends FindByName with WithNames {
  def displayName: String = names.doFind(Language.Russian.toSpec).name

  def structureName: String = names.doFind(Language.English.toSpec).name
}

object Store {

  type Path = Seq[Store]

  def findByName[M](
    fullName: String,
    allowedExtension: String,
    findByName: String => Caching.Parser[Option[M]],
    assumeAllowedExtension: Boolean = false
  ): Caching.Parser[Option[M]] = {
    val (fileName: String, extension: Option[String]) = Files.nameAndExtension(fullName)

    val name: Option[String] =
      if (extension.isDefined && !extension.contains(allowedExtension))
        if (assumeAllowedExtension) Some(fullName) else None
      else
        Some(fileName)

    name.fold[Caching.Parser[Option[M]]](ZIO.none)(name => findByName(name))
  }

  def findByName(name: String, stores: Seq[Store]): Parser[Option[Store]] =
    ZIO.succeed(stores.find(_.names.hasName(name)))

  def renderXml[T <: Store](elements: Elements[T], value: T): String = renderXml(elements.xmlElement(value))
  def renderXml(element: ScalaXml.Element): String = prettyPrinter.renderWithHeader(ScalaXml)(element)

  val prettyPrinter: PrettyPrinter = new PrettyPrinter(
    nestElements = Set("p"), // TODO remnants of TEI?
    alwaysStackElements = Set("store", "by")
  )
}
