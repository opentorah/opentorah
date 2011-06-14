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

package org.podval.judaica.viewer

import scala.xml.Node


class Books {

    def get(request: Request): Node = {
        toHtml(request.contextPath, <p>Herehere!</p>)
    }


    private def toHtml(baseUrl: String, body: Seq[Node]): Node = {
        <html>
            <head>
                <link href={baseUrl + "style.css"} rel="stylesheet" type="text/css"/>
            </head>
            <body>
                {body}
            </body>
        </html>
    }
}
