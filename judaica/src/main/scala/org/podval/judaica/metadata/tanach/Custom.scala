package org.podval.judaica.metadata.tanach

import org.podval.judaica.metadata.{NamedCompanion, Named, Names, Util}

// Assumptions: no cycles; only Common doesn't have parent.
sealed class Custom(val parent: Option[Custom]) extends Named {
  final override def names: Names = Custom.toNames(this)

  final lazy val children: Set[Custom] = Custom.values.filter(_.parent.contains(this)).toSet

  final def isLeaf: Boolean = children.isEmpty
}

// TODO we need some consistency in the naming of customs: if Bavlim, then maybe Sefaradim?
object Custom extends NamedCompanion {

  override type Key = Custom

  class Of[T](val customs: Map[Custom, T]) {
    final def find(custom: Custom): Option[T] =
      customs.get(custom).orElse(custom.parent.flatMap(find))

    final def doFind(custom: Custom): T = {
      val result = find(custom)
      require(result.nonEmpty, s"Missing custom: $custom")
      result.get
    }

    final def common: T = doFind(Common)

    final def findKey(custom: Custom): Option[Custom] =
      if (customs.contains(custom)) Some(custom) else custom.parent.flatMap(findKey)

    final def doFindKey(custom: Custom): Custom = {
      val result = findKey(custom)
      require(result.nonEmpty, s"Missing custom: $custom")
      result.get
    }

    final def toLeaves: Of[T] =
      new Of(leaves.flatMap(custom => find(custom).map(custom -> _)).toMap)

    final def contains(custom: Custom): Boolean = find(custom).isDefined

    final def keySet: Set[Custom] = leaves.filter(contains)

    final def notCovered: Set[Custom] = leaves -- keySet

    final def verifyFull: Of[T] = {
      leaves.foreach(doFind)
      this
    }

    final def minimize: Of[T] = {
      this // TODO
    }

//    private def addParents: Of[T] = {
//      def addOneParent: (Of[T], Boolean) = values
//        .find { custom =>
//          !customs.contains(custom) && !custom.isLeaf && Util.contains(customs.keySet, custom.children)
//        }
//        .fold((customs, false))(missing => (customs + missing, true))
//
//      val (result, added) = addOneParent
//      if (!added) result else result.addParents
//    }

    final def lift[Q, R](b: Of[Q], f: (Custom, Option[T], Option[Q]) => R): Map[Custom, R] =
      lift[Q, Option[T], Option[Q], R](b, f, _.find(_), _.find(_))

    final def liftL[Q, R](b: Of[Q], f: (Custom, T, Option[Q]) => R): Map[Custom, R] =
      lift[Q, T, Option[Q], R](b, f, _.doFind(_), _.find(_))

    final def liftLR[Q, R](b: Of[Q], f: (Custom, T, Q) => R): Map[Custom, R] =
      lift[Q, T, Q, R](b, f, _.doFind(_), _.doFind(_))

    private def lift[Q, TA, QA, R](
      b: Of[Q],
      f: (Custom, TA, QA) => R,
      tf: (Of[T], Custom) => TA,
      qf: (Of[Q], Custom) => QA
    ): Map[Custom, R] =
      leaves.map { custom => custom -> f(custom, tf(this, custom), qf(b, custom)) }.toMap

    final def lift[R](f: (Custom, Option[T]) => R): Map[Custom, R] =
      leaves.map { custom => custom -> f(custom, find(custom)) }.toMap

    final def ++(other: Of[T]): Of[T] = new Of[T](customs ++ other.customs)

    final def *(other: Of[T]): Of[(T, Option[T])] =
      new Of[(T, Option[T])](liftL[T, (T, Option[T])](other, { case (_: Custom, a: T, b: Option[T]) => (a, b) }))
  }

  object Of {
    def apply[T](value: T): Custom.Of[T] = new Of[T](Map(Common -> value))

    def apply[T](map: Sets[T], full: Boolean = true): Of[T] = {
      // Check that the sets do not overlap.
      val sets: Set[Set[Custom]] = map.keySet
      sets.foreach(a => sets.foreach(b => if (b != a) {
        require(b.intersect(a).isEmpty, s"Overlaping sets of customs: $a and $b")
      }))

      // TODO -?
      Util.checkNoDuplicates(map.values.toSeq, "customs")

      val result = new Of[T](map.flatMap { case (customs, value) => customs.map(custom => custom -> value) })
      if (full) result.verifyFull else result
    }
  }

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

  val leaves: Set[Custom] = values.toSet.filter(_.isLeaf)

  def parse(names: String): Set[Custom] = {
    val result: Seq[Custom] = names.split(',').map(_.trim).map(getForName)
    Util.checkNoDuplicates(result, "customs")
    result.toSet
  }

  def common[T](map: Sets[T]): T = map(map.keySet.find(_.contains(Custom.Common)).get)
}
