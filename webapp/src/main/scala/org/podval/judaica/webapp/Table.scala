/*
 *  Copyright 2013-2014 Leonid Dubinsky <dub@podval.org>.
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

package org.podval.judaica.webapp

import javax.ws.rs.core.{UriInfo, UriBuilder}
import scala.xml.{Text, Node, Elem}


abstract class Column[T](val name: String) {
  def apply(value: T, uriBuilder: UriBuilder): Node
}


abstract class TextColumn[T](name: String) extends Column[T](name){
  final override def apply(value: T, uriBuilder: UriBuilder): Node = Text(text(value))
  def text(value: T): String
}


abstract class LinkColumn[T](name: String) extends Column[T](name) {
  final override def apply(value: T, uriBuilder: UriBuilder): Node = <a href={link(value, uriBuilder).build().toString}>{text(value)}</a>
  def link(value: T, uriBuilder: UriBuilder): UriBuilder
  def text(value: T): String
}


abstract class SimpleLinkColumn[T](name: String) extends LinkColumn[T](name) {
  final override def link(value: T, uriBuilder: UriBuilder): UriBuilder = uriBuilder.path(text(value))
}



object Table {

  def apply[T](data: Seq[T], uriInfo: UriInfo, columns: Column[T]*): Elem = {
    val uriBuilder = uriInfo.getAbsolutePathBuilder
    <table>
      <thead>
        <tr>{columns.map { column =>
          <th>{column.name}</th>
        }}
        </tr>
      </thead>
      { data.map { value =>
        <tr>{columns.map { column =>
          <td>{column(value, UriBuilderCloner.copy(uriBuilder))}</td>
        }}
        </tr>
      }}
    </table>
  }
}
