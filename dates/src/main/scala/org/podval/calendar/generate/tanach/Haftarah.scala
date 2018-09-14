package org.podval.calendar.generate.tanach

import org.podval.calendar.metadata.LanguageSpec

final class Haftarah(
  val parsha: Parsha,
  val customs: Custom.Of[Seq[NachSpan]]
) {
  override def toString: String = toString(LanguageSpec.empty)

  def toString(spec: LanguageSpec): String = {
    customs.toSeq.map { case (custom, spans) =>
      custom.toString(spec) + ": " + NachSpan.toString(spans, spec)
    }.mkString("\n")
  }
}

object Haftarah {
  private val toHaftarah: Map[Parsha, Haftarah] = HaftarahMetadataParser.parse

  def forParsha(parsha: Parsha): Haftarah = toHaftarah(parsha)
}
