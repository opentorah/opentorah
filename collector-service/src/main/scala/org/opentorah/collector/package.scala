package org.opentorah

import zio.RIO
import zio.blocking.Blocking
import zio.clock.Clock
import zio.console.Console

package object collector {
  type ServiceEnvironment = Console with Clock with Blocking

  type ServiceTask[A] = RIO[ServiceEnvironment, A]
}
