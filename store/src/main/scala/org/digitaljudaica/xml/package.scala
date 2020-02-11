package org.digitaljudaica

import cats.data.StateT

package object xml {
  type Error = String

  type ErrorOr[A] = Either[Error, A]

  type Parser[A] = StateT[ErrorOr, Context, A]

  type Ops = Ops.Ops
}
