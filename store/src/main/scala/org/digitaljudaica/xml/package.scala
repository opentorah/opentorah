package org.digitaljudaica

import zio.ZIO

package object xml {

  type Error = String

  // TODO eliminate
  type ErrorOr[A] = Either[Error, A]

  type Parser[A] = ZIO[Context, Error, A]
}
