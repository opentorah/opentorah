package org.opentorah.util

object Collections {
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

  def removeConsecutiveDuplicates[T](seq: Seq[T]): Seq[T] =
    removeConsecutiveDuplicates[T, T](Seq.empty, seq.toList)(identity)

  def removeConsecutiveDuplicatesWith[T, D](seq: Seq[T])(f: T => D): Seq[T] =
    removeConsecutiveDuplicates[T, D](Seq.empty, seq.toList)(f)

  @scala.annotation.tailrec
  private def removeConsecutiveDuplicates[T, D](result: Seq[T], seq: List[T])(f: T => D): Seq[T] = seq match {
    case x :: y :: xs =>
      removeConsecutiveDuplicates(
        if (f(x) == f(y)) result else result :+ x,
        y :: xs
      )(f)
    case x :: Nil => result :+ x
    case Nil => result
  }

  // b.intersect(a) == b?
  def contains[T](a: Set[T], b: Set[T]): Boolean = b.forall(t => a.contains(t))

  def inSequence[K, V, R](keys: Seq[K], map: Map[K, V], f: Seq[(K, V)] => Seq[R]): Map[K, R] =
    keys.zip(f(keys.map { key => key -> map(key) })).toMap

  def mapValues[A, B, C](map: Map[A, B])(f: B => C): Map[A, C] =
    map.view.mapValues(f).toMap // TODO for Scala 2.13
    //map.mapValues(f) // Scala 2.12

  def pruneSequenceOfMaps[K, A, B](tail: Seq[(K, Map[A, B])]): Seq[(K, Map[A, B])] = pruneSequenceOfMaps(Seq.empty, tail)

  @scala.annotation.tailrec
  def pruneSequenceOfMaps[K, A, B](acc: Seq[(K, Map[A, B])], tail: Seq[(K, Map[A, B])]): Seq[(K, Map[A, B])] = tail match {
    case Nil => acc
    case (key, map) :: nextTail => pruneSequenceOfMaps(acc :+ (key, map -- nextTail.flatMap(_._2.keys).toSet), nextTail)
  }

  // where is this in the standard library?
  def compare(a: Option[String], b: Option[String]): Int = {
    if (a.isEmpty && b.isEmpty) 0
    else if (a.isEmpty) -1
    else if (b.isEmpty) 1
    else a.get.compare(b.get)
  }

  def prevAndNext[A](as: Seq[A]): Seq[(A, (Option[A], Option[A]))] = if (as.isEmpty) Seq.empty else {
    val options: Seq[Option[A]] = as.map(Some(_))
    val prev = None +: options.init
    val next = options.tail :+ None
    as.zip(prev.zip(next))
  }
}
