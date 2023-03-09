package org.opentorah.build

import org.gradle.api.Project

open class JavaDependencyRequirement(
  dependency: JavaDependency,
  version: String,
  reason: String,
  configurations: Configurations,
  isVersionExact: Boolean = false
) extends DependencyRequirement(
  dependency,
  version,
  reason,
  configurations,
  isVersionExact
):
  override protected def getAddition(project: Project): Dependency.WithVersion =
    dependency.apply(
      version = version
    )
