package org.podval.docbook.gradle

import org.gradle.api.provider.MapProperty
import org.gradle.api.tasks.{Copy, Input}
import org.apache.tools.ant.filters.ReplaceTokens
import scala.beans.BeanProperty
import scala.collection.JavaConverters._

class FilteringCopyTask extends Copy {
  @Input @BeanProperty val tokens: MapProperty[String, String] =
    getProject.getObjects.mapProperty(classOf[String], classOf[String])

  override def copy(): Unit = {
    filter(Map("tokens" -> tokens.get).asJava, classOf[ReplaceTokens])
    super.copy()
  }
}
