package org.opentorah.util

object Collections:
  // Will this *ever* be in the standard library?
  // Or am I supposed to just use Cats - where, I am sure, it exists?

  // TODO in Scala 2.13+ - use LazyList:
  //  def unfold[A, B](start: A)(f: A => Option[(A, B)]): Stream[B] =
  //    f(start).map { case (a, b) => b #:: unfold(a)(f) }.getOrElse(Stream.empty)
  //
  //  def unfoldInfinite[A, B](start: A)(f: A => (A, B)): Stream[B] =
  //    f(start) match { case (a, b) => b #:: unfoldInfinite(a)(f) }

  private def unfoldInfiniteSimple[A](start: A, next: A => A): LazyList[A] =
    start #:: unfoldInfiniteSimple(next(start), next)

  def unfoldSimple[A](start: A, next: A => A, take: A => Boolean): Seq[A] =
    unfoldInfiniteSimple(start, next).takeWhile(take).toList

  // TODO what is this in pointless notation?
  def concat[A, B](fs: Seq[A => Seq[B]]): A => Seq[B] = a => fs.flatMap(f => f(a))

  // Group consecutive elements with the same key - didn't find this in the standard library.
  def group[T, K](list: Seq[T], key: T => K)(using CanEqual[K, K]): Seq[Seq[T]] = if list.isEmpty then Nil else
    val k: K = key(list.head)
    val (ks: Seq[T], notks: Seq[T]) = list.span(key(_) == k)
    Seq(ks) ++ group(notks, key)

  private def duplicates[T](seq: Seq[T]): Set[T] = seq.groupBy(t => t).filter((_, ts) => ts.length > 1).keySet

  def checkNoDuplicates[T](seq: Seq[T], what: String): Unit =
    val result = duplicates(seq)
    require(result.isEmpty, s"Duplicate $what: $result")
  
  def inSequence[K, V, R](keys: Seq[K], map: Map[K, V], f: Seq[(K, V)] => Seq[R]): Map[K, R] =
    keys.zip(f(keys.map(key => key -> map(key)))).toMap

  def mapValues[A, B, C](map: Map[A, B])(f: B => C): Map[A, C] =
    map.view.mapValues(f).toMap // Scala 2.13
