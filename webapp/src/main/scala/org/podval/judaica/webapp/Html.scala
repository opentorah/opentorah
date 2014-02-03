/*
 *  Copyright 2011-2014 Leonid Dubinsky <dub@podval.org>.
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
 * under the License.
 */

package org.podval.judaica.webapp

import javax.ws.rs.core.UriInfo

import scala.xml.Elem
import java.net.URI


object Html {

  def apply(uriInfo: UriInfo, what: Elem): Elem = apply(uriInfo, StyleSheets.get(uriInfo), what)


  private def apply(uriInfo: UriInfo, stylesheets: Seq[URI], what: Elem): Elem = {
    <html>
      <head>
        {stylesheets.map(uri => <link rel="stylesheet" type="text/css" href={uri.toString}/>)}
      </head>
      <body>
        {what}
      </body>
    </html>
  }
}
