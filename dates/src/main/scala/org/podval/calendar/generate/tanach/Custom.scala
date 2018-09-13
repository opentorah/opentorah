package org.podval.calendar.generate.tanach

import org.podval.calendar.metadata.{WithNames, WithNamesCompanion}

// I don't think it worth it to move parent definitions into the XML file...
sealed class Custom(parent: Option[Custom]) extends WithNames[Custom] {
  final override def companion: WithNamesCompanion[Custom] = Custom
}

object Custom extends WithNamesCompanion[Custom] {
  case object Ashkenaz extends Custom(None)
  case object Italki extends Custom(Some(Ashkenaz))
  case object Frankfurt extends Custom(Some(Ashkenaz))
  case object Lita extends Custom(Some(Ashkenaz))
  case object Hayey_odom extends Custom(Some(Lita))
  case object Hagra extends Custom(Some(Ashkenaz))

  case object Sefard extends Custom(None)
  case object Chabad extends Custom(Some(Sefard))
  case object Magreb extends Custom(Some(Sefard))
  case object Algeria extends Custom(Some(Magreb))
  case object Toshbim extends Custom(Some(Magreb))
  case object Bavlim extends Custom(Some(Sefard))
  case object Teiman extends Custom(Some(Sefard))
  case object Baladi extends Custom(Some(Teiman))
  case object Shami extends Custom(Some(Teiman))

  override val values: Seq[Custom] = Seq(
    Ashkenaz, Italki, Frankfurt, Lita, Hayey_odom, Hagra,
    Sefard, Chabad, Magreb, Algeria, Toshbim, Bavlim, Teiman, Baladi, Shami)
}
