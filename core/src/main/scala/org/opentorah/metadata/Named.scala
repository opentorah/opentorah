package org.opentorah.metadata

import org.opentorah.util.Effects

// TODO rename HasNames
trait Named:
  def names: Names

  def merge(that: Named): Named =
    require(this eq that)
    this

  final def toLanguageString(using spec: Language.Spec): String = names.toLanguageString(using spec)

  // TODO introduce NamesDecorator

  final def andNumber(number: Int): Named =
    new Named.Numbers(this, number, number):
      override protected def suffix(languageSpec: Language.Spec): String =
        languageSpec.toString(number)

  final def andNumbers(from: Int, to: Int): Named = if to == from then andNumber(from) else
    new Named.Numbers(this, from, to):
      override protected def suffix(languageSpec: Language.Spec): String =
        languageSpec.toString(this.from) + "-" + languageSpec.toString(this.to)

object Named:

  // Note: Calendar.Month is used as a Names.Loader, but this gets called during its initialization,
  // so loader parameter end up being null... Introduced thunk:
  abstract class ByLoader[Key <: ByLoader[Key]](loader: => Names.Loader[Key], nameOverride: Option[String])
    extends Named, HasName(nameOverride):
    final override def names: Names = loader.toNames(this.asInstanceOf[Key]) // TODO play with self-type to remove the cast...

  private sealed abstract class Numbers(
    val  named: Named,
    val from: Int,
    val to: Int
  ) extends Named:
    require(from > 0)
    require(to >= from)

    final override def names: Names = new Names(for name <- named.names.names yield Name(
      name = name.name + " " + suffix(name.languageSpec),
      languageSpec = name.languageSpec
    ))

    protected def suffix(languageSpec: Language.Spec): String

    override def merge(other: Named): Named =
      require(other.isInstanceOf[Numbers])
      val that: Numbers = other.asInstanceOf[Numbers]
      require(this.named eq that.named)
      require(this.to+1 == that.from)
      this.named.andNumbers(this.from, that.to)
