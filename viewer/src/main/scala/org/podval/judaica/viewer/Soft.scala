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


final class Soft[T] private(gen: => T) {

  private[this] var cache = new java.lang.ref.SoftReference(null.asInstanceOf[T])


  def apply(): T = {
    val value = cache.get
    if (value != null) value else {
      cache = new java.lang.ref.SoftReference(gen)
      cache.get
    }
  }


  def get: T = apply()
}



object Soft {

  def apply[T](gen: => T) = new Soft(gen)
}
