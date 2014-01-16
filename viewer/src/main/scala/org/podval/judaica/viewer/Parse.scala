/*
 *  Copyright 2014 Leonid Dubinsky <dub@podval.org>.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.podval.judaica.viewer


object Parse {

  def sequence[X, K, T](f : (K, X) => T)(n: (K, T) => K)(known: K, rest: Seq[X]): Seq[T] = {
    def parse(known: K, rest: Seq[X]): Seq[T] = rest match {
      case Nil => Nil
      case x :: xs =>
        val result: T = f(known, x)
        val nextKnown: K = n(known, result)
        result +: parse(nextKnown, xs)
    }

    parse(known, rest)
  }
}
