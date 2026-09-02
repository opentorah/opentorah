package org.opentorah.texts.tanach

import org.opentorah.metadata.{HasName, HasValues, Named, Names}
import org.opentorah.util.Collections
import org.opentorah.xml.{Attribute, ElementTo, From, Parsable, Parser, Unparser}

// The hierarchy lives in CustomTree.xml, not here. Assumptions: no cycles;
// only Common has no parent.
enum Custom(nameOverride: Option[String] = None)
  extends Named.ByLoader[Custom](loader = Custom, nameOverride), HasName.Enum derives CanEqual:
  // lazy: resolving a parent needs the names, which the loader reads lazily.
  lazy val parent: Option[Custom] = Custom.parentOf(this)

  lazy val children: Set[Custom] = Custom.valuesSeq.filter(_.parent.contains(this)).toSet

  /** Is this the given custom, or one that inherits from it? */
  def isUnder(ancestor: Custom): Boolean =
    (this == ancestor) || parent.exists(_.isUnder(ancestor))

  def level: Int = parent.fold(0)(parent => parent.level+1)

  case Common extends Custom()
  case Ashkenaz extends Custom()
  case Italki extends Custom()
  case Frankfurt extends Custom()
  case Poznan extends Custom()
  case Lita extends Custom()
  case ChayeyOdom extends Custom(nameOverride = Some("Chayey Odom"))
  case Hagra extends Custom()
  case Sefard extends Custom()
  case Chabad extends Custom()
  case PureSephardim extends Custom(nameOverride = Some("Pure Sephardim"))
  case Persia extends Custom()
  case Libya extends Custom()
  case Magreb extends Custom()
  case Algeria extends Custom()
  case Algiers extends Custom()
  case Toshbim extends Custom()
  case Djerba extends Custom()
  case Morocco extends Custom()
  case Fes extends Custom()
  case Marrakesh extends Custom()
  case Agadir extends Custom()
  case Bavlim extends Custom()
  case Teiman extends Custom()
  case Baladi extends Custom()
  case Shami extends Custom()
  case Romania extends Custom()


object Custom extends Names.Loader[Custom], HasValues.FindByName[Custom]:
  override val valuesSeq: Seq[Custom] = values.toIndexedSeq

  /**
   * The hierarchy, read from CustomTree.xml rather than declared in the enum,
   * so that it can be edited as data. Lazy for the same reason the names are:
   * resolving an entry needs the names, which the loader reads on demand.
   */
  private object TreeEntry extends ElementTo[(String, Option[String])]("custom"):
    override def contentParsable: Parsable[(String, Option[String])] =
      new Parsable[(String, Option[String])]:
        override def parser: Parser[(String, Option[String])] = for
          name: String <- Attribute("n").required()
          parent: Option[String] <- Attribute("parent").optional()
        yield (name, parent)
        override def unparser: Unparser[(String, Option[String])] = ???

  private lazy val parents: Map[Custom, Option[Custom]] =
    val from: From = From.resourceNamed(this, "CustomTree")
    val entries: Seq[(String, Option[String])] =
      Parser.unsafeRun(TreeEntry.wrappedSeq(from.name).parse(from))
    val byName: Map[String, Custom] = valuesSeq.map(custom => custom.name -> custom).toMap
    val missing: Seq[String] = valuesSeq.map(_.name).filterNot(entries.map(_._1).contains)
    require(missing.isEmpty, s"CustomTree.xml does not mention: ${missing.mkString(", ")}")
    entries.map((name, parent) =>
      val custom: Custom = byName.getOrElse(name, throw IllegalArgumentException(s"Unknown custom: $name"))
      custom -> parent.map(p => byName.getOrElse(p, throw IllegalArgumentException(s"Unknown parent: $p")))
    ).toMap

  private[tanach] def parentOf(custom: Custom): Option[Custom] = parents(custom)

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

    @scala.annotation.targetName("append")
    final def ++(other: Of[T]): Of[T] = new Of[T](customs ++ other.customs, full = false)

    /**
     * This, but with the subtree under each of `at` taken from `other` instead:
     * used where one custom follows a different reading from everyone else, as
     * Chabad does when Acharei and Kedoshim combine. Everything at or below a
     * grafted custom comes from `other`, so grafting at Common replaces the lot.
     */
    final def graft(other: Of[T], at: Set[Custom]): Of[T] = if at.isEmpty then this else
      val grafted: Customs[T] = at.foldLeft(Map.empty[Custom, T])((acc, root) =>
        acc ++ other.customs.filter((custom, _) => custom.isUnder(root)) + (root -> other.doFind(root))
      )
      new Of[T](customs.filterNot((custom, _) => at.exists(custom.isUnder)) ++ grafted, full = false)

    @scala.annotation.targetName("multiply")
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
