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

import org.junit.Test
import org.junit.Assert._

import java.io.File


class WorksTest {

    private val sourceDirectory = new File(System.getProperty("project.build.sourceDirectory"))


    private val resourcesDirectory = new File(sourceDirectory.getParent, "resources/org/podval/judaica/viewer/")


    private val works = Works(resourcesDirectory)


    @Test
    def findChumash = assertTrue(works.getByName("Chumash").isDefined)
}
