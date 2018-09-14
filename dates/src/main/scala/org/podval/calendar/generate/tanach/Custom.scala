package org.podval.calendar.generate.tanach

import org.podval.calendar.metadata.{WithNames, WithNamesCompanion}

// I don't think it worth it to move parent definitions into the XML file...
sealed class Custom(val parent: Option[Custom]) extends WithNames[Custom] {
  final override def companion: WithNamesCompanion[Custom] = Custom
}

object Custom extends WithNamesCompanion[Custom] {
  case object Common extends Custom(None)
    case object Ashkenaz extends Custom(Some(Common))
      case object Italki extends Custom(Some(Ashkenaz))
      case object Frankfurt extends Custom(Some(Ashkenaz))
      case object Lita extends Custom(Some(Ashkenaz))
        case object Hayey_odom extends Custom(Some(Lita))
      case object Hagra extends Custom(Some(Ashkenaz))
    case object Sefard extends Custom(Some(Common))
      case object Chabad extends Custom(Some(Sefard))
      case object Magreb extends Custom(Some(Sefard))
        case object Algeria extends Custom(Some(Magreb))
        case object Toshbim extends Custom(Some(Magreb))
        case object Djerba extends Custom(Some(Magreb))
      case object Bavlim extends Custom(Some(Sefard))
      case object Teiman extends Custom(Some(Sefard))
        case object Baladi extends Custom(Some(Teiman))
        case object Shami extends Custom(Some(Teiman))

  override val values: Seq[Custom] = Seq(
    Common, Ashkenaz, Italki, Frankfurt, Lita, Hayey_odom, Hagra,
    Sefard, Chabad, Magreb, Algeria, Toshbim, Djerba, Bavlim, Teiman, Baladi, Shami)

  type Of[T] = Map[Custom, T]


  def find[T](customs: Of[T], custom: Custom): Custom = {
    val result: Option[T] = customs.get(custom)
    if (result.isDefined) custom else {
      require(custom.parent.isDefined)
      find(customs, custom.parent.get)
    }
  }

  def get[T](customs: Of[T], custom: Custom): T = customs(find(customs, custom))
}
