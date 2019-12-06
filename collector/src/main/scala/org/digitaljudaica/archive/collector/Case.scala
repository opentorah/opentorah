package org.digitaljudaica.archive.collector

final class Case(val name: String) extends Ordered[Case] {
  if (name.isEmpty) throw new IllegalArgumentException

  private val (prefix: String, number: Int) =
    if (name.startsWith("=")) ("=", name.substring(1).toInt)
    else name.toIntOption.fold((name, 0))(number => ("", number))

  override def toString: String = name

  override def compare(that: Case): Int = {
    val result: Int = prefix.compare(that.prefix)
    if (result != 0) result else number.compare(that.number)
  }
}
