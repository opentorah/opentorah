package org.opentorah.store

import org.opentorah.xml.{Attribute, ElementTo, ElementsTo, Parsable, Parser, Unparser}
import java.net.URL

// TODO site?
final class WithSource[T](val source: String, val value: T)

object WithSource:

  final class Of[T](elementsTo: ElementsTo[T]) extends ElementTo[WithSource[T]]("withSource"):
    private val valueElement: ElementsTo.Required[T] = elementsTo.required

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
        valueElement(_.value)/////.removeNamespace()
      )

  private val sourceAttribute: Attribute.Required[String] = Attribute("sourceUrl").required

  def apply[T](
    url: URL,
    name: String,
    value: ElementsTo[T]
  ): ListFile[WithSource[T], Seq[WithSource[T]]] = ListFile[WithSource[T], Seq[WithSource[T]]](
    url,
    name,
    entry = Of[T](value),
    wrapper = identity
  )
