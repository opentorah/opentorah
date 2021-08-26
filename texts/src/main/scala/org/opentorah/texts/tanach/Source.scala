package org.opentorah.texts.tanach

import org.opentorah.metadata.{Named, Names}

object Source {
  trait Numbers extends Named {
    def withNames: Named

    def from: Int

    def to: Int

    override def merge(other: Named): Named = {
      require(other.isInstanceOf[Numbers])
      val that = other.asInstanceOf[Numbers]
      require(this.withNames == that.withNames)
      require(this.to+1 == that.from)
      new AndNumbers(this.withNames, this.from, that.to)
    }
  }

  final class AndNumber(override val withNames: Named, val number: Int) extends Numbers {
    require(number > 0)

    override def from: Int = number

    override def to: Int = number

    override def names: Names = withNames.names.transform(name => name.copy(
      name.name + " " + name.languageSpec.toString(number)
    ))
  }

  final class AndNumbers(override val withNames: Named, override val from: Int, override val to: Int) extends Numbers {
    require(from > 0)
    require(to > from)

    override def names: Names = withNames.names.transform(name => name.copy(
      name.name + " " + name.languageSpec.toString(from) + "-" + name.languageSpec.toString(to)
    ))
  }
}
