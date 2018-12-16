package org.podval.judaica.tanach

import org.podval.judaica.metadata.{Named, NamedCompanion, Names}
import org.podval.judaica.util.Util

// Assumptions: no cycles; only Common doesn't have parent.
sealed class Custom(val parent: Option[Custom]) extends Named {
  final override def names: Names = Custom.toNames(this)

  final lazy val children: Set[Custom] = Custom.values.filter(_.parent.contains(this)).toSet

  final def level: Int = parent.fold(0)(parent => parent.level+1)
}

object Custom extends NamedCompanion {

  override type Key = Custom

  class Of[T](val customs: Map[Custom, T], full: Boolean = true) {
    if (full) require(isFull)

    final def find(custom: Custom): Option[T] =
      customs.get(custom).orElse(custom.parent.flatMap(find))

    final def doFind(custom: Custom): T = {
      val result = find(custom)
      require(result.nonEmpty, s"Missing custom: $custom")
      result.get
    }

    final def common: T = doFind(Common)

    final def commonOnly: Option[T] =
      find(Common).flatMap(common => if (customs.size == 1) Some(common) else None)

    final def isFull: Boolean = all.forall(custom => find(custom).isDefined)

    final def maximize: Map[Custom, T] = all.map(custom => custom -> doFind(custom)).toMap

    final def minimize: Of[T] = new Of[T](Of.minimize(maximize))

    final def lift[Q, R](b: Of[Q], f: (Custom, Option[T], Option[Q]) => R): Of[R] =
      lift[Q, Option[T], Option[Q], R](b, f, _.find(_), _.find(_))

    final def liftL[Q, R](b: Of[Q], f: (Custom, T, Option[Q]) => R): Of[R] =
      lift[Q, T, Option[Q], R](b, f, _.doFind(_), _.find(_))

    final def liftLR[Q, R](b: Of[Q], f: (Custom, T, Q) => R): Of[R] =
      lift[Q, T, Q, R](b, f, _.doFind(_), _.doFind(_))

    private def lift[Q, TA, QA, R](
      b: Of[Q],
      f: (Custom, TA, QA) => R,
      tf: (Of[T], Custom) => TA,
      qf: (Of[Q], Custom) => QA
    ): Of[R] =
      new Of[R](all.map { custom => custom -> f(custom, tf(this, custom), qf(b, custom)) }.toMap)

    final def lift[R](f: (Custom, Option[T]) => R): Of[R] =
      new Of[R](all.map { custom => custom -> f(custom, find(custom)) }.toMap)

    final def liftR[R](f: (Custom, T) => R): Of[R] =
      new Of[R](all.map { custom => custom -> f(custom, doFind(custom)) }.toMap)

    final def ++(other: Of[T]): Of[T] = new Of[T](customs ++ other.customs)

    final def *(other: Of[T]): Of[(T, Option[T])] =
      liftL[T, (T, Option[T])](other, { case (_: Custom, a: T, b: Option[T]) => (a, b) })
  }

  object Of {
    def apply[T](value: T): Custom.Of[T] = new Of[T](Map(Common -> value))

    def apply[T](pairs: Seq[(Set[Custom], T)], full: Boolean): Of[T] = {
      Util.checkNoDuplicates(pairs.map(_._1), "pre-map Sets[T]")
      apply(pairs.toMap, full)
    }

    def apply[T](map: Sets[T], full: Boolean = true): Of[T] = {
      // Check that the sets do not overlap.
      val sets: Set[Set[Custom]] = map.keySet
      sets.foreach(a => sets.foreach(b => if (b != a) {
        require(b.intersect(a).isEmpty, s"Overlaping sets of customs: $a and $b")
      }))

      Util.checkNoDuplicates(map.values.toSeq, "customs")

      new Of[T](map.flatMap { case (customs, value) => customs.map(custom => custom -> value) }, full = full)
    }

    // go through levels of Customs (real) in descending order;
    // each level only affects the next one, not the preceding ones;
    // customs on the same level do not affect one another.
    private val byLevelDescending: Seq[Custom] = all.toSeq.sortBy(_.level).reverse

    private def minimize[T](customs: Map[Custom, T]): Map[Custom, T] = {
      // start with maximized representation: all Customs other than Common present;
      val result: Map[Custom, T] =
        byLevelDescending.foldLeft(customs) { case (customs: Map[Custom, T], custom: Custom) =>
          if (custom.children.isEmpty) customs else {
            customs.get(custom).fold(customs) { value =>
              customs -- custom.children.filter(customs(_) == value)
            }
          }
        }

      val commonValue: Option[T] = if (result.keySet != Common.children) None else {
        val values: Set[T] = result.values.toSet
        if (values.size != 1) None else Some(values.head)
      }

      commonValue.fold[Map[Custom, T]](result)(commonValue => Map(Common -> commonValue))
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
    Common,
    Ashkenaz, Italki, Frankfurt, Lita, ChayeyOdom, Hagra,
    Sefard, Chabad,
    Magreb, Algeria, Toshbim, Djerba, Morocco, Fes, Bavlim, Teiman, Baladi, Shami)

  val all: Set[Custom] = values.toSet.filter(_.parent.isDefined)

  def parse(names: String): Set[Custom] = {
    val result: Seq[Custom] = names.split(',').map(_.trim).map(getForName)
    Util.checkNoDuplicates(result, "customs")
    result.toSet
  }

  def common[T](map: Sets[T]): T = map(map.keySet.find(_.contains(Custom.Common)).get)
}
