package org.podval.judaica.metadata.tanach

import org.podval.judaica.metadata.{NamedCompanion, Named, Names, Util}

sealed class Custom(val parent: Option[Custom]) extends Named {
  final override def names: Names = Custom.toNames(this)

  lazy val children: Set[Custom] = Custom.values.filter(_.parent.contains(this)).toSet
}

// TODO we need some consistency in the naming of customs: if Bavlim, then maybe Sefaradim?
object Custom extends NamedCompanion {
  // I don't think it worth it to move parent definitions into the XML file...
  // child < parent does not induce total order...

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
        case object Fes extends Custom(Some(Magreb))
      case object Bavlim extends Custom(Some(Sefard))
      case object Teiman extends Custom(Some(Sefard))
        case object Baladi extends Custom(Some(Teiman))
        case object Shami extends Custom(Some(Teiman))

  override val values: Seq[Custom] = Seq(
    Common, Ashkenaz, Italki, Frankfurt, Lita, Hayey_odom, Hagra,
    Sefard, Chabad, Magreb, Algeria, Toshbim, Djerba, Fes, Bavlim, Teiman, Baladi, Shami)

  type Of[T] = Map[Custom, T]

  def ofCommon[T](value: T): Custom.Of[T] = Map(Common -> value)

  def find[T](customs: Of[T], custom: Custom): Custom = find(customs.keySet, custom)

  def find(customs: Set[Custom], custom: Custom): Custom = {
    if (customs.contains(custom)) custom else {
      require(custom.parent.isDefined, s"Custom $custom is missing from $customs")
      find(customs, custom.parent.get)
    }
  }

  // Parse and normalize a set of customs:
  // - add customs that have all children already in the set (recursively);
  // - remove customs that have parents in the set.
  def parse(names: String): Set[Custom] = {
    val customs: Seq[Custom] = names.split(' ').map(_.trim).map(getForName)
    Util.checkNoDuplicates(customs, "customs")

    val result = addParents(customs.toSet)
    result.filterNot(custom => custom.parent.isDefined && result.contains(custom.parent.get))
  }

  private def addParents(customs: Set[Custom]): Set[Custom] = {
    val (result, added) = addOneParent(customs)
    if (!added) result else addParents(result)
  }

  private def addOneParent(customs: Set[Custom]): (Set[Custom], Boolean) = values
    .find { custom =>
      val children = custom.children
      !customs.contains(custom) && children.nonEmpty && Util.contains(customs, children)
    }
    .fold((customs, false))(missing => (customs + missing, true))

  type Sets[T] = Map[Set[Custom], T]

  def denormalize[T](map: Sets[T]): Of[T] = {
    check(map)
    Util.checkNoDuplicates(map.values.toSeq, "customs")
    map.flatMap { case (customs, value) => customs.map(custom => custom -> value) }
  }

  // Check that the sets do not overlap - and cover all customs.
  def check[T](map: Sets[T]): Unit = {
    val sets: Set[Set[Custom]] = map.keySet
    sets.foreach(a => sets.foreach(b => if (b != a) {
      require(b.intersect(a).isEmpty, s"Overlaping sets of customs: $a and $b")
    }))
    val all: Set[Custom] = addParents(sets.flatten)
    values.foreach(custom => find(all, custom))
  }

  def common[T](map: Sets[T]): T = map(map.keySet.find(_.contains(Custom.Common)).get)
}
