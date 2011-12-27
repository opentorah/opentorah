/*
 *  Copyright 2011 Leonid Dubinsky <dub@podval.org>.
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

import org.fusesource.scalate.TemplateEngine


class Books {

    private val works = Works("/home/dub/code/podval-judaica/viewer/src/main/resources/org/podval/judaica/viewer/")


    private val engine = new TemplateEngine


    def get(request: Request): Node = {

        def getAllWorks: Seq[Node] = {
            <h1>All Works</h1>
            <table>{
                works.get.map(work =>
                    {
                        val nameO = work.names.getByLang("en")
                        val name = if (nameO.isDefined) nameO.get else work.names.getDefault
                        <tr>
                            <td>
                                <a href={request.basePath + "/" + work.names.getDefault.name}>{name.name}</a>
                            </td>
                        </tr>
                    }
                )
            }</table>
        }


        def getWork(name: String): Seq[Node] = {
            val workO = works.getByName(name)

            if (workO.isEmpty) {
                throw new IllegalArgumentException("Not found: " + name) // TODO use NotFoundException for this!
            }

            val work = workO.get

            <h1>{work.names.getDefault.name}</h1>
        }


        <html>
            <head>
            </head>
            <body>{
                if (request.isPathEmpty) getAllWorks else getWork(request.popPath)
            }</body>
        </html>
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
