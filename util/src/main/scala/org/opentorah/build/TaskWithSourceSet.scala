package org.opentorah.build

import org.gradle.api.Task
import org.gradle.api.file.FileCollection
import org.gradle.api.tasks.{Classpath, SourceSet}
import org.opentorah.build.Gradle.*

trait TaskWithSourceSet extends Task:
  protected final def sourceSet: SourceSet = getProject.getSourceSet(sourceSetName)

  protected def sourceSetName: String

  @Classpath final def getRuntimeClassPath: FileCollection = sourceSet.getRuntimeClasspath
