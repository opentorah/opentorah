package org.opentorah.texts.tanach

import org.opentorah.metadata.{HasName, HasValues, Named, Names}
import org.opentorah.util.Collections

// Assumptions: no cycles; only Common doesn't have parent.
enum Custom(val parent: Option[Custom], nameOverride: Option[String] = None)
  extends Named.ByLoader[Custom](loader = Custom, nameOverride), HasName.Enum derives CanEqual:
  lazy val children: Set[Custom] = Custom.valuesSeq.filter(_.parent.contains(this)).toSet

  def level: Int = parent.fold(0)(parent => parent.level+1)

  case Common extends Custom(None)
    case Ashkenaz extends Custom(Some(Common))
      case Italki extends Custom(Some(Ashkenaz))
      case Frankfurt extends Custom(Some(Ashkenaz))
      case Lita extends Custom(Some(Ashkenaz))
        case ChayeyOdom extends Custom(Some(Lita), nameOverride = Some("Chayey Odom"))
      case Hagra extends Custom(Some(Ashkenaz))
    case Sefard extends Custom(Some(Common))
      case Chabad extends Custom(Some(Sefard))
      case Magreb extends Custom(Some(Sefard))
        case Algeria extends Custom(Some(Magreb))
        case Toshbim extends Custom(Some(Magreb))
        case Djerba extends Custom(Some(Magreb))
        case Morocco extends Custom(Some(Magreb))
          case Fes extends Custom(Some(Morocco))
      case Bavlim extends Custom(Some(Sefard))
      case Teiman extends Custom(Some(Sefard))
        case Baladi extends Custom(Some(Teiman))
        case Shami extends Custom(Some(Teiman))

object Custom extends Names.Loader[Custom], HasValues.FindByName[Custom]:
  override val valuesSeq: Seq[Custom] = values.toIndexedSeq

  val all: Set[Custom] = values.toSet.filter(_.parent.isDefined)

  type Customs[T] = Map[Custom, T]

  type Sets[T] = Map[Set[Custom], T]

  open class Of[T](val customs: Customs[T], full: Boolean = true):
    if full then require(isFull)

    final def find(custom: Custom): Option[T] =
      customs.get(custom).orElse(custom.parent.flatMap(find))

    final def doFind(custom: Custom): T =
      val result = find(custom)
      require(result.nonEmpty, s"Missing custom: $custom")
      result.get

    final def common: T = doFind(Common)

    final def commonOnly: Option[T] =
      find(Common).flatMap(common => if customs.size == 1 then Some(common) else None)

    final def isFull: Boolean = all.forall(custom => find(custom).isDefined)

    final def maximize: Customs[T] = all.map(custom => custom -> doFind(custom)).toMap

    final def minimize(using CanEqual[T, T]): Of[T] = new Of[T](Of.minimize(maximize))

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
      new Of[R](all.map(custom => custom -> f(custom, tf(this, custom), qf(b, custom))).toMap)

    final def lift[R](f: (Custom, Option[T]) => R): Of[R] =
      new Of[R](all.map(custom => custom -> f(custom, find(custom))).toMap)

    final def liftR[R](f: (Custom, T) => R): Of[R] =
      new Of[R](all.map(custom => custom -> f(custom, doFind(custom))).toMap)

    final def map[R](f: T => R, full: Boolean = true): Of[R] =
      new Of[R](Collections.mapValues(customs)(f), full = full)

    final def ++(other: Of[T]): Of[T] = new Of[T](customs ++ other.customs, full = false)

    final def *(other: Of[T]): Of[(T, Option[T])] =
      liftL[T, (T, Option[T])](other, (_: Custom, a /*: T*/, b: Option[T]) => (a, b))

  object Of:
    def apply[T](customs: Customs[T]): Custom.Of[T] = new Of[T](customs)

    def apply[T](value: T): Custom.Of[T] = new Of[T](Map(Common -> value))

    def apply[T](pairs: Seq[(Set[Custom], T)], full: Boolean): Of[T] =
      Collections.checkNoDuplicates(pairs.map(_._1), "pre-map Sets[T]")
      apply(pairs.toMap, full)

    def apply[T](map: Sets[T], full: Boolean = true): Of[T] =
      // Check that the sets do not overlap.
      val sets: Set[Set[Custom]] = map.keySet
      sets.foreach(a => sets.foreach(b => if b != a then
        require(b.intersect(a).isEmpty, s"Overlaping sets of customs: $a and $b")
      ))

      Collections.checkNoDuplicates(map.values.toSeq, "customs")

      new Of[T](map.flatMap((customs, value) => customs.map(custom => custom -> value)), full = full)

    // go through levels of Customs (real) in descending order;
    // each level only affects the next one, not the preceding ones;
    // customs on the same level do not affect one another.
    private val byLevelDescending: Seq[Custom] = all.toSeq.sortBy(_.level).reverse

    private def minimize[T](customs: Customs[T])(using CanEqual[T, T]): Customs[T] =
      // start with maximized representation: all Customs other than Common present;
      val result: Customs[T] =
        byLevelDescending.foldLeft(customs)((customs: Customs[T], custom: Custom) =>
          if custom.children.isEmpty then customs else 
            customs.get(custom).fold(customs)(value =>
              customs -- custom.children.filter(customs(_) == value)
            )
        )

      val commonValue: Option[T] = if result.keySet != Common.children then None else
        val values: Set[T] = result.values.toSet
        if values.size != 1 then None else Some(values.head)

      commonValue.fold[Customs[T]](result)(commonValue => Map(Common -> commonValue))
  
  def parse(names: String): Set[Custom] =
    val result: Seq[Custom] = names.split(',').toIndexedSeq.map(_.trim).map(getForName)
    Collections.checkNoDuplicates(result, "customs")
    result.toSet

  def common[T](map: Sets[T]): T = map(map.keySet.find(_.contains(Custom.Common)).get)
