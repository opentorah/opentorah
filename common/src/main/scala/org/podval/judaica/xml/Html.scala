/*
 *  Copyright 2011-2013 Leonid Dubinsky <dub@podval.org>.
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

package org.podval.judaica.xml

import scala.xml.{Elem, Text}


object Html {

  def html(stylesheet: String, what: Seq[Elem]) =
    <html>
      <head>
        <link rel="stylesheet" type="text/css" href={stylesheet + ".css"}/>
      </head>
      <body class="hebrew">
        {what}
      </body>
    </html>


  def span(class_ : String, text: String): Elem = <span class={class_}>{text}</span>


  def text(what: String) = Text(what)
}
