package org.digitaljudaica

import cats.implicits._
import cats.data.StateT

package object xml {
  type Error = String

  type ErrorOr[A] = Either[Error, A]

  type Parser[A] = StateT[ErrorOr, Context, A]

  type Operations = Ops.Ops

  private[xml] def pure[A](value: A): Parser[A] = StateT.pure[ErrorOr, Context, A](value)

  private[xml] def lift[A](value: ErrorOr[A]): Parser[A] = StateT.liftF[ErrorOr, Context, A](value)

  private[xml] def inspect[A](f: Context => A): Parser[A] = StateT.inspect[ErrorOr, Context, A](f)

  private[xml] def set(context: Context): Parser[Unit] = StateT.set[ErrorOr, Context](context)

  private[xml] def modify(f: Context => Context): Parser[Unit] = StateT.modify[ErrorOr, Context](f)
}
