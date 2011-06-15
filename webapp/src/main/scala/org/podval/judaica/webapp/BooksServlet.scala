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

package org.podval.judaica.webapp

import org.podval.judaica.viewer.{Books, Request}

import javax.servlet.ServletException
import javax.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse, HttpUtils}

import java.io.{OutputStream, IOException}

import java.util.Hashtable

import scala.collection.JavaConversions.mapAsScalaMap

import scala.xml.{Node, PrettyPrinter}


final class BooksServlet extends HttpServlet {

    private val books = new Books


    @throws(classOf[ServletException])
    @throws(classOf[IOException])
    protected override def doGet(httpRequest: HttpServletRequest, response: HttpServletResponse) {
        val request = parseRequest(httpRequest)

        val result = books.get(request)

        // TODO: prettyprint only if requested through query parameter!
        write(result, response.getOutputStream)
    }


//    // "application/x-www-form-urlencoded"
//    def put(form: Map[String, Seq[String]]) = {
//        // TODO
//    }

    private def parseRequest(request: HttpServletRequest): Request =
        new Request(
            emptyIfNull(request.getContextPath),
            request.getServletPath,
            request.getPathInfo,
            parseQuery(request)
        )


    private def parseQuery(request: HttpServletRequest): Map[String, Seq[String]] = {
        val queryString = emptyIfNull(request.getQueryString)
        Map[String, Seq[String]]() ++
            (mapAsScalaMap(HttpUtils.parseQueryString(queryString).asInstanceOf[Hashtable[String, Array[String]]]).mapValues(_.toSeq))
    }


    private def emptyIfNull(what: String): String = if (what == null) "" else what


    private def write(nodes: Node, out: OutputStream) {
        var answer = prettyPrinter.format(nodes)
        out.write(answer.getBytes())
        out.write('\n')
    }
 
 
    private val prettyPrinter = new PrettyPrinter(120, 4)
}
