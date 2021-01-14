package org.opentorah.node

import java.lang.reflect.Field
import com.eclipsesource.v8.V8
import org.opentorah.util.Collections.mapValues
import org.slf4j.{Logger, LoggerFactory}
import scala.jdk.CollectionConverters._

final class J2V8(libraryPath: String) {

  override def toString: String = s"J2V8 library $libraryPath"

  def load(): Boolean = {
    try {
      System.load(libraryPath)

      J2V8.logger.info(s"Loaded $this")
      val field: Field = classOf[V8].getDeclaredField("nativeLibraryLoaded")
      field.setAccessible(true)
      field.set(null, true)
      true
    } catch {
      case e: UnsatisfiedLinkError =>
        J2V8.logger.warn(s"Failed to load $this: ${e.getMessage}")
        false
    }
  }
}

object J2V8 {
  private val logger: Logger = LoggerFactory.getLogger(classOf[J2V8])

  def map2java(map: Map[String, Any]): java.util.Map[String, Any] =
    mapValues(map)(value2java).asJava

  def list2java(list: List[Any]): java.util.List[Any] =
    list.map(value2java).asJava

  private def value2java(value: Any): Any = value match {
    // with value: Map[String, Any] I get:
    //   non-variable type argument String in type pattern scala.collection.immutable.Map[String,Any]
    //   (the underlying of Map[String,Any]) is unchecked since it is eliminated by erasure
    case value: Map[_, Any] => map2java(value.asInstanceOf[Map[String, Any]])
    case value: List[Any] => list2java(value)
    case other => other
  }
}
