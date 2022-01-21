package org.opentorah.store

import org.opentorah.xml.{Attribute, Element, Elements, Parsable, Parser, Unparser}
import java.net.URL

final class WithSource[T](val source: String, val value: T)

object WithSource:

  final class Of[T](elements: Elements[T]) extends Element[WithSource[T]]("withSource"):
    private val valueElement: Elements.Required[T] = elements.required

    override def contentParsable: Parsable[WithSource[T]] = new Parsable[WithSource[T]]:
      override protected def parser: Parser[WithSource[T]] = for
        source: String <- WithSource.sourceAttribute()
        value: T <- valueElement()
      yield new WithSource[T](
        source,
        value
      )

      override def unparser: Unparser[WithSource[T]] = Unparser.concat[WithSource[T]](
        WithSource.sourceAttribute(_.source),
        (valueElement(_.value) : Unparser[WithSource[T]])/////.removeNamespace() // TODO why do I need an ascription here?
      )

  private val sourceAttribute: Attribute.Required[String] = Attribute("sourceUrl").required

  def apply[T](
    url: URL,
    name: String,
    value: Elements[T]
  ): ListFile[WithSource[T], Seq[WithSource[T]]] = ListFile[WithSource[T], Seq[WithSource[T]]](
    url,
    name,
    entry = Of[T](value),
    wrapper = identity
  )
