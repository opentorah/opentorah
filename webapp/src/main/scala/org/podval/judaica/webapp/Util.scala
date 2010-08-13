/*
 * Copyright 2010 dub.
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


object Util {

    def toPairs(form: MultivaluedMap[String, String]): Set[Pair[String, String]] = {
        for (entry <- form.entrySet) yield {
            val name: String = entry.getKey()
            val values = entry.getValue()
            if (values.size > 1) {
                
            }

            new Pair(name, values(0))
        }
    }


    def toHtml(baseUrl: String, body: Seq[Node]): Seq[Node] = {
        <html>
            <head>
                <link href={baseUrl + "style.css"} rel="stylesheet" type="text/css"/>
            </head>
            <body>
                {what}
            </body>
        </html>
    }
}
