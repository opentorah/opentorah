package org.podval.judaica.metadata.tanach

import org.podval.judaica.metadata.{NamedCompanion, Named, Names, Util}

// Assumptions: no cycles; only Common doesn't have parent.
sealed class Custom(val parent: Option[Custom]) extends Named {
  final override def names: Names = Custom.toNames(this)

  final lazy val children: Set[Custom] = Custom.values.filter(_.parent.contains(this)).toSet

  final def isLeafe: Boolean = children.isEmpty
}

// TODO we need some consistency in the naming of customs: if Bavlim, then maybe Sefaradim?
object Custom extends NamedCompanion {

  override type Key = Custom

  type Of[T] = Map[Custom, T]

  type Sets[T] = Map[Set[Custom], T]

  case object Common extends Custom(None)
    case object Ashkenaz extends Custom(Some(Common))
      case object Italki extends Custom(Some(Ashkenaz))
      case object Frankfurt extends Custom(Some(Ashkenaz))
      case object Lita extends Custom(Some(Ashkenaz))
        case object ChayeyOdom extends Custom(Some(Lita)) { override def name: String = "Chayey Odom" }
      case object Hagra extends Custom(Some(Ashkenaz))
    case object Sefard extends Custom(Some(Common))
      // According to some, this is prevaling Chabad custom:
      case object RavNaeHolyLand extends Custom(Some(Chabad)) { override def name: String = "Rav Nae Holy Land" }
      case object Chabad extends Custom(Some(Sefard))
      case object Magreb extends Custom(Some(Sefard))
        case object Algeria extends Custom(Some(Magreb))
        case object Toshbim extends Custom(Some(Magreb))
        case object Djerba extends Custom(Some(Magreb))
        case object Morocco extends Custom(Some(Magreb))
          case object Fes extends Custom(Some(Morocco))
      case object Bavlim extends Custom(Some(Sefard))
      case object Teiman extends Custom(Some(Sefard))
        case object Baladi extends Custom(Some(Teiman))
        case object Shami extends Custom(Some(Teiman))

  override val values: Seq[Custom] = Seq(
    Common, Ashkenaz, Italki, Frankfurt, Lita, ChayeyOdom, Hagra,
    Sefard, RavNaeHolyLand, Chabad,
    Magreb, Algeria, Toshbim, Djerba, Morocco, Fes, Bavlim, Teiman, Baladi, Shami)

  val leaves: Set[Custom] = values.toSet.filter(_.isLeafe)

  def common[T](value: T): Custom.Of[T] = Map(Common -> value)

  def find[T](customs: Of[T], custom: Custom): T = customs(effective(customs, custom))

  def effective[T](customs: Of[T], custom: Custom): Custom = doFind(customs.keySet, custom)

  def lift[A, B, C](a: Of[A], b: Of[B], f: (Custom, A, B) => C): Of[C] = leaves.map { custom =>
    custom -> f(custom, find(a, custom), find(b, custom))
  }.toMap // TODO minimize?

  private def doFind(customs: Set[Custom], custom: Custom): Custom = {
    if (customs.contains(custom)) custom else {
      require(custom.parent.isDefined, s"Custom $custom is missing from $customs")
      doFind(customs, custom.parent.get)
    }
  }

  // Parse and normalize a set of customs:
  // - add customs that have all children already in the set (recursively);
  // - remove customs that have parents in the set.
  def parse(names: String): Set[Custom] = {
    val customs: Seq[Custom] = names.split(',').map(_.trim).map(getForName)
    Util.checkNoDuplicates(customs, "customs")

    val result = addParents(customs.toSet)
    result.filterNot(custom => custom.parent.isDefined && result.contains(custom.parent.get))
  }

  private def addParents(customs: Set[Custom]): Set[Custom] = {
    def addOneParent(customs: Set[Custom]): (Set[Custom], Boolean) = values
      .find { custom =>
        !customs.contains(custom) && !custom.isLeafe && Util.contains(customs, custom.children)
      }
      .fold((customs, false))(missing => (customs + missing, true))

    val (result, added) = addOneParent(customs)
    if (!added) result else addParents(result)
  }


  def denormalize[T](map: Sets[T], full: Boolean): Of[T] = {
    check(map, full)
    Util.checkNoDuplicates(map.values.toSeq, "customs")
    map.flatMap { case (customs, value) => customs.map(custom => custom -> value) }
  }

  // Check that the sets do not overlap - and cover all customs.
  private def check[T](map: Sets[T], full: Boolean): Unit = {
    val sets: Set[Set[Custom]] = map.keySet
    sets.foreach(a => sets.foreach(b => if (b != a) {
      require(b.intersect(a).isEmpty, s"Overlaping sets of customs: $a and $b")
    }))
    val all: Set[Custom] = addParents(sets.flatten)
    if (full) values.foreach(custom => doFind(all, custom))
  }

  def common[T](map: Sets[T]): T = map(map.keySet.find(_.contains(Custom.Common)).get)
}
