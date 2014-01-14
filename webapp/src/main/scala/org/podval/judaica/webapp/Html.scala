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

import org.podval.judaica.viewer.Work
import scala.xml.Elem
import javax.ws.rs.core.UriInfo
import java.io.File


object Html {

  def apply(uriInfo: UriInfo, workOption: Option[Work], what: Elem): Elem = {
    val mainStylesheet: String = uriInfo.getBaseUriBuilder.
      path("works").
      path("stylesheet.css").
      build().toString

    val workStylesheet: Option[String] = {
      if (workOption.isEmpty) None else {
        val work = workOption.get
        val file = new File(work.directory, "stylesheet.css")
        if (!file.exists) None else {
          val uri: String = uriInfo.getBaseUriBuilder.
            path("works").
            path(work.names.default.name).
            path("stylesheet.css").
            build().toString

          Some(uri)
        }
      }
    }

    // TODO add the Edition stylesheet (different fonts for different languages etc.)

    <html>
      <head>
        <link rel="stylesheet" type="text/css" href={mainStylesheet}/>
        {if (workStylesheet.isDefined)
          <link rel="stylesheet" type="text/css" href={workStylesheet.get}/>
        }
      </head>
      <body>
        {what}
      </body>
    </html>
  }
}
