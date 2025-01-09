package org.opentorah.build

trait DependencyCoordinates:
  def group: String
  def artifact: String
  def classifier(version: Version): Option[String]
  def extension(version: Version): Option[String]
  