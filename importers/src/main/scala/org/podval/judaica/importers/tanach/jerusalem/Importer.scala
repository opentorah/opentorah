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

package org.podval.judaica.importers.tanach.jerusalem

import java.io.File


class Importer(inputDirectory: String, metadataDirectory: String, outputDirectory: String) {

    def run() {
        importBook("bereishis", "Genesis");
//        importBook("shemos", "Exodus");
//        importBook("vayikro", "Leviticus");
//        importBook("bamidbor", "Numbers");
//        importBook("devorim", "Deuteronomy");
    }


    private def importBook(inputName: String, outputName: String) {
        val xml = new Book(
            new File(inputDirectory, inputName+".txt"),
            new File(metadataDirectory, outputName+".xml"),
            new File(outputDirectory, outputName+".xml")
        ).run();
    }
}
