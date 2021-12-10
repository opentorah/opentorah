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

  def unfoldInfiniteSimple[A](start: A, next: A => A): LazyList[A] =
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

  def removeConsecutiveDuplicatesWith[T, D](seq: Seq[T])(f: T => D)(using CanEqual[D, D]): Seq[T] =
    removeConsecutiveDuplicates[T, D](Seq.empty, seq.toList)(f)

  @scala.annotation.tailrec
  private def removeConsecutiveDuplicates[T, D](result: Seq[T], seq: List[T])(f: T => D)(using CanEqual[D, D]): Seq[T] =
    if seq.isEmpty then result else
    if seq.length == 1 then result :+ seq.head else
      val x: T = seq.head
      val y: T = seq.tail.head
      removeConsecutiveDuplicates(
        if f(x) == f(y) then result else result :+ x,
        seq.tail
      )(f)

  // b.intersect(a) == b?
  def contains[T](a: Set[T], b: Set[T]): Boolean = b.forall(t => a.contains(t))

  def inSequence[K, V, R](keys: Seq[K], map: Map[K, V], f: Seq[(K, V)] => Seq[R]): Map[K, R] =
    keys.zip(f(keys.map(key => key -> map(key)))).toMap

  def mapValues[A, B, C](map: Map[A, B])(f: B => C): Map[A, C] =
    map.view.mapValues(f).toMap // Scala 2.13
    //map.mapValues(f) // Scala 2.12

  // where is this in the standard library?
  def compare(a: Option[String], b: Option[String]): Int =
    if a.isEmpty && b.isEmpty then 0
    else if a.isEmpty then -1
    else if b.isEmpty then 1
    else a.get.compare(b.get)

  def prevAndNext[A](as: Seq[A]): Seq[(A, (Option[A], Option[A]))] = if as.isEmpty then Seq.empty else
    val options: Seq[Option[A]] = as.map(Some(_))
    val prev = None +: options.init
    val next = options.tail :+ None
    as.zip(prev.zip(next))
