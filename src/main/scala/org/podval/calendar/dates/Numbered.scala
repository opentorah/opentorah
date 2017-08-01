/*
 * Copyright 2011 Podval Group.
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

package org.podval.calendar.dates


abstract class Numbered[T <: Numbered[T]](val number: Int) extends Ordered[T] {

    final override def equals(other: Any): Boolean = other match {
        case that: Numbered[_] => number == that.number
        case _ => false
    }

    final override def hashCode: Int = number

    final override def compare(that: T): Int = this.number - that.number

    override def toString: String = number.toString
}
