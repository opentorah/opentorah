package org.podval.calendar.generate.tanach

object Util {
  // Will this *ever* be in the standard library?
  // Or am I supposed to just use Cats - where, I am sure, it exists?

  //  def unfold[A, B](start: A)(f: A => Option[(A, B)]): Stream[B] =
  //    f(start).map { case (a, b) => b #:: unfold(a)(f) }.getOrElse(Stream.empty)
  //
  //  def unfoldInfinite[A, B](start: A)(f: A => (A, B)): Stream[B] =
  //    f(start) match { case (a, b) => b #:: unfoldInfinite(a)(f) }

  def unfoldInfiniteSimple[A](start: A)(f: A => A): Stream[A] = start #:: unfoldInfiniteSimple(f(start))(f)
}
