package org.opentorah.store

import java.net.URL
import org.opentorah.util.Util
import org.opentorah.xml.{Antiparser, Attribute, Element, Parser, ToXml}
import scala.xml.{Elem, Node}

abstract class ComponentNg(val elementName: String) {

  type Instance <: ComponentNg.Base

  protected def parser(
    urls: Urls,
    inheritedSelectors: Seq[Selector]
  ): Parser[Instance]

  final def toXml(value: Instance): Elem = toXml.toXmlElement(value)

  private val toXml: ToXml[Instance] = new ToXml[Instance] {
    override protected def elementName(value: Instance): String = ComponentNg.this.elementName

    override protected def antiparser: Antiparser[Instance] = Antiparser(
      attributes = value =>
        ComponentNg.typeAttribute.toXml.compose[Instance](value => Util.getSingletonClassName(value.companion)).attributes(value) ++
        attributes(value),
      content = ComponentNg.this.content
    )
  }

  protected def attributes(value: Instance): Seq[Attribute.Value[_]]

  // TODO take Inline descendants into account!
  protected def content(value: Instance): Seq[Node]
}

object ComponentNg {

  abstract class Base(
    final val urls: Urls,
    inheritedSelectors: Seq[Selector]
  ) {
    protected def definedSelectors: Seq[Selector] = Seq.empty

    final def selectors: Seq[Selector] = inheritedSelectors ++ definedSelectors

    final def selectorByName(name: String): Selector = selectors.find(_.names.hasName(name)).get

    def companion: ComponentNg
  }

  private val typeAttribute: Attribute[String] = Attribute("type")

  def read(
    elementName: String,
    fromUrl: URL,
    inheritedSelectors: Seq[Selector] = Selector.predefinedSelectors
  ): Parser[Base] = read(
    elementName,
    Urls.fromUrl(fromUrl),
    inheritedSelectors
  )

  private def read(
    elementName: String,
    fromUrl: Urls.FromUrl,
    inheritedSelectors: Seq[Selector]
  ): Parser[Base] = {
    val parsable = new Element[Base](elementName) {
      override protected def parser: Parser[Base] = for {
        file <- Redirect.fileAttribute.optional
        result <-
          if (file.isDefined) read(
            elementName,
            fromUrl.redirect(file.get),
            inheritedSelectors
          ) else for {
            className <- typeAttribute.required
            result <- Util.getSingleton(className).asInstanceOf[ComponentNg].parser(
              fromUrl,
              inheritedSelectors
            )
          } yield result
      } yield result
    }
    parsable.parse(fromUrl.baseUrl)
  }
}
