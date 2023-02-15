package org.opentorah.schedule.tanach

import org.opentorah.calendar.jewish.{Jewish, YearType}
import org.opentorah.texts.tanach.Parsha
import org.opentorah.texts.tanach.Parsha.*
import YearType.*

object ReadingStructure:
  // This table gives parsha combinations for all occurring year types.
  // Submitted by @michaelko58; sourced from  https://www.shoresh.org.il/spages/articles/parashathibur.htm
  // Primary/classical source needs to be determined.
  sealed trait Combine:
    final def combined(parsha: Parsha, inHolyLand: Boolean): Option[Parsha] =
      if combine(inHolyLand) then Some(parsha) else None

    def combine(inHolyLand: Boolean): Boolean

  case object M extends Combine: // Mehubarim: combined
    def combine(inHolyLand: Boolean): Boolean = true

  case object N extends Combine: // Nifradim: separate
    def combine(inHolyLand: Boolean): Boolean = false

  case object C extends Combine: // mehubarim Chutz lo'oretz: combined in diaspora
    def combine(inHolyLand: Boolean): Boolean = !inHolyLand

  final class S
  (
    val vp: Combine, // Vayakhel/Pekudei
    val tm: Combine, // Tazria/Metzora
    val ak: Combine, // Acharei/Kedoshim
    val bb: Combine, // Behar/Bechukosai
    val cb: Combine, // Chukas/Balak
    val mm: Combine, // Mattos/Masai
    val nv: Combine  // Nitzavim/Vayeilech
  ):
    def combined(inHolyLand: Boolean): Seq[Parsha] = Seq[(Combine, Parsha)](
      vp -> Vayakhel,
      tm -> Tazria,
      ak -> Acharei,
      bb -> Behar,
      cb -> Chukas,
      mm -> Mattos,
      nv -> Nitzavim
    ).flatMap((c, p) => c.combined(p, inHolyLand))

  val all: Map[YearType, S] = Map(
    // non-leap
    N2S -> S(vp = M, tm = M, ak = M, bb = M, cb = N, mm = M, nv = M),
    N2F -> S(vp = M, tm = M, ak = M, bb = M, cb = C, mm = M, nv = M),
    N3R -> S(vp = M, tm = M, ak = M, bb = M, cb = C, mm = M, nv = M),
    N5R -> S(vp = M, tm = M, ak = M, bb = C, cb = N, mm = M, nv = N),
    N5F -> S(vp = N, tm = M, ak = M, bb = M, cb = N, mm = M, nv = N),
    N7S -> S(vp = M, tm = M, ak = M, bb = M, cb = N, mm = M, nv = N),
    N7F -> S(vp = M, tm = M, ak = M, bb = M, cb = N, mm = M, nv = M),

    // leap
    L2S -> S(vp = N, tm = N, ak = N, bb = N, cb = C, mm = M, nv = M),
    L2F -> S(vp = N, tm = N, ak = N, bb = N, cb = N, mm = C, nv = N),
    L3R -> S(vp = N, tm = N, ak = N, bb = N, cb = N, mm = C, nv = N),
    L4S -> S(vp = N, tm = N, ak = N, bb = N, cb = N, mm = N, nv = N),
    L5F -> S(vp = N, tm = N, ak = N, bb = N, cb = N, mm = N, nv = M),
    L7S -> S(vp = N, tm = N, ak = N, bb = N, cb = N, mm = M, nv = M),
    L7F -> S(vp = N, tm = N, ak = N, bb = N, cb = C, mm = M, nv = M)
  )
