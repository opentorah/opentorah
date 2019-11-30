package org.podval.fop.mathjax

import java.lang.reflect.Field

import org.podval.fop.util.Util.mapValues
import com.eclipsesource.v8.V8
import org.podval.fop.util.Logger

import scala.jdk.CollectionConverters._

final class J2V8(libraryPath: String) {
  def load(logger: Logger): Boolean = {
    try {
      System.load(libraryPath)

      logger.info(s"Loaded J2V8 library $libraryPath")
      val field: Field = classOf[V8].getDeclaredField("nativeLibraryLoaded")
      field.setAccessible(true)
      field.set(null, true)
      true
    } catch {
      case e: UnsatisfiedLinkError =>
        logger.warn(s"Failed to load J2V8 library $libraryPath: ${e.getMessage}")
        false
    }
  }
}

object J2V8 {

  def map2java(map: Map[String, Any]): java.util.Map[String, Any] =
    mapValues(map)(value2java).asJava

  def list2java(list: List[Any]): java.util.List[Any] =
    list.map(value2java).asJava

  private def value2java(value: Any): Any = value match {
    case value: Map[String, Any] => map2java(value)
    case value: List[Any] => list2java(value)
    case other => other
  }
}
