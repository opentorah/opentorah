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

  def sequence[X, C, T](f : (C, X) => T)(n: (C, T) => C)(context: C, xs: Seq[X]): Seq[T] =
    xs.foldLeft((Seq.empty[T], context)) { case ((result, context), x) =>
        val one = f(context, x)
        (result :+ one, n(context, one))
    }._1
}
