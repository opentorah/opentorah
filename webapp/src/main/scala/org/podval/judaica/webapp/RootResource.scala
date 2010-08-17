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

import org.podval.judaica.viewer.{Viewer, TextDescriptor}
import org.podval.judaica.viewer.tanach.TanachTextFormat

import javax.ws.rs.{Path, GET, POST, Produces, Consumes}
import javax.ws.rs.core.{Context, UriInfo, MultivaluedMap}

import scala.collection.JavaConversions._

import java.io.File


@Path("/")
final class RootResource {

//            new Text("/var/www/sites/hg.judaica/projects/texts/Tanach/jerusalem/Genesis.xml"),
//            new Text("/var/www/sites/hg.judaica/projects/texts/Tanach/toronto/Genesis.xml")

    val viewer = new Viewer(
            new TanachTextFormat(),
            "chapter",
            List(
                new TextDescriptor("jerusalem", false, "/tmp/j.xml"),
                new TextDescriptor("toronto", true, "/tmp/t.xml")
            )
        )
 
// @todo blog about the spaces in Scala's XML literals if curly braces are not flush with the tags!
//        val trimmed = merged.flatMap(Utility.trimProper(_))
//        
// Also, about utility of the Seq[Node] writer...

    @GET
    @Produces(Array("text/html"))
    def get(@Context uri: UriInfo) = {
        val baseUrl: String = uri.getBaseUri().toString
        Util.toHtml(baseUrl, viewer.get(baseUrl))
    }


    @POST
    @Consumes(Array("application/x-www-form-urlencoded"))
    @Produces(Array("text/plain"))
    def post(@Context form: MultivaluedMap[String, String]) = viewer.put(asMap(form).asInstanceOf[Map[String, List[String]]])


    @GET
    @Path("style.css")
    @Produces(Array("text/css"))
    def getStylesheet() = {
        // @todo locate the file in the way appropriate to the environment!
        new File("/tmp/style.css")
    }
}
