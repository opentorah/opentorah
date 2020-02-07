package org.digitaljudaica.util

object Collections {
  // Will this *ever* be in the standard library?
  // Or am I supposed to just use Cats - where, I am sure, it exists?

  //  def unfold[A, B](start: A)(f: A => Option[(A, B)]): Stream[B] =
  //    f(start).map { case (a, b) => b #:: unfold(a)(f) }.getOrElse(Stream.empty)
  //
  //  def unfoldInfinite[A, B](start: A)(f: A => (A, B)): Stream[B] =
  //    f(start) match { case (a, b) => b #:: unfoldInfinite(a)(f) }

  def unfoldInfiniteSimple[A](start: A, next: A => A): Stream[A] =
    start #:: unfoldInfiniteSimple(next(start), next)

  def unfoldSimple[A](start: A, next: A => A, take: A => Boolean): Seq[A] =
    unfoldInfiniteSimple(start, next).takeWhile(take).toList

  // Group consecutive elements with the same key - didn't find this in the standard library.
  def group[T, K](list: Seq[T], key: T => K): Seq[Seq[T]] = if (list.isEmpty) Nil else {
    val k = key(list.head)
    val (ks, notks) = list.span(key(_) == k)
    Seq(ks) ++ group(notks, key)
  }

  def duplicates[T](seq: Seq[T]): Set[T] = seq.groupBy(t => t).filter { case (_, ts) => ts.length > 1 }.keySet

  def checkNoDuplicates[T](seq: Seq[T], what: String): Unit = {
    val result = duplicates(seq)
    require(result.isEmpty, s"Duplicate $what: $result")
  }

  def removeConsecutiveDuplicates[T](seq: Seq[T]): Seq[T] = removeConsecutiveDuplicates(Seq.empty, seq.toList)

  @scala.annotation.tailrec
  private def removeConsecutiveDuplicates[T](result: Seq[T], seq: List[T]): Seq[T] = seq match {
    case x :: y :: xs =>
      removeConsecutiveDuplicates(
        if (x == y) result else result :+ x,
        y :: xs
      )
    case x :: Nil => result :+ x
    case Nil => result
  }

  // b.intersect(a) == b?
  def contains[T](a: Set[T], b: Set[T]): Boolean = b.forall(t => a.contains(t))

  def inSequence[K, V, R](keys: Seq[K], map: Map[K, V], f: Seq[(K, V)] => Seq[R]): Map[K, R] =
    keys.zip(f(keys.map { key => key -> map(key) })).toMap

  def mapValues[A, B, C](map: Map[A, B])(f: B => C): Map[A, C] =
  // map.view.mapValues(f).toMap // Scala 2.13
    map.mapValues(f) // Scala 2.12
}
