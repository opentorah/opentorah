/*
 *  Copyright 2013 Leonid Dubinsky <dub@podval.org>.
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

import org.podval.judaica.viewer.{Works, Named}
import scala.xml.Elem
import javax.ws.rs.core.UriBuilder


object Table {

  def build[T <: Named](data: Seq[T], uriBuilder: UriBuilder, suffix: Option[String]): Elem =
    <table>{
      data.map { named =>
        val name = named.names.default.name
        val beforeSuffix = UriBuilderCloner.copy(uriBuilder).path(name)
        val uri = (if (suffix.isEmpty) beforeSuffix else beforeSuffix.path(suffix.get)).build()

        <tr>
          <td>
            <a href={uri.toString} >{name}</a>
          </td>
        </tr>
      }
    }</table>
}
