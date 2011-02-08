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

package org.podval.judaica.importers


object Main {

    def main(args: Array[String]) {
//        new tanach.jerusalem.JerusalemTanachImporter(
//            "/home/dub/projects-judaica/imports/Tanach/jerusalem",
//            "/tmp/xxx"
//        )
        new chabad.ChabadImporter(
            // There is stuff one level up also
            "/mnt/data/www.chabadlibrary.org/books2/",
            "/tmp/xxx"

        )
        .run()
    }
}
