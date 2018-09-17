package org.podval.judaica.metadata.tanach

import org.podval.judaica.metadata.NamesLoader

object Custom extends NamesLoader {
  // I don't think it worth it to move parent definitions into the XML file...
  sealed class Custom(val parent: Option[Custom]) extends KeyBase

  override type Key = Custom

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

  def find[T](customs: Of[T], custom: Custom): Custom = find(customs.keySet, custom)

  def find(customs: Set[Custom], custom: Custom): Custom = {
    if (customs.contains(custom)) custom else {
      require(custom.parent.isDefined)
      find(customs, custom.parent.get)
    }
  }

  def parse(names: String): Set[Custom] = {
    val customs: Seq[Custom] = names.split(' ').map(_.trim).map(getForName)
    require(customs.length == customs.toSet.size, s"Duplicate customs: $customs")
    val result = customs.toSet

    // TODO add customs that have all children already in the set - recursively.

    // Remove customs that have parents in the set.
    result.filterNot(custom => custom.parent.isDefined && result.contains(custom.parent.get))
  }

  def checkDisjoint(sets: Set[Set[Custom]]): Unit = {
    // TODO
  }
}
