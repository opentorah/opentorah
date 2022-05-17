package org.opentorah.node

import com.eclipsesource.v8.V8
import org.opentorah.util.Collections.mapValues
import org.slf4j.{Logger, LoggerFactory}
import java.io.File
import java.lang.reflect.Field
import scala.jdk.CollectionConverters.*

final class J2V8(val library: File):

  override def toString: String = s"J2V8 library $library"

  def load(): Boolean =
    try
      // TODO prevent double-load
      System.load(library.getAbsolutePath)

      J2V8.logger.info(s"Loaded $this")
      val field: Field = classOf[V8].getDeclaredField("nativeLibraryLoaded")
      field.setAccessible(true)
      field.set(null, true)
      true
    catch
      case e: UnsatisfiedLinkError =>
        J2V8.logger.warn(s"Failed to load $this: ${e.getMessage}")
        false

object J2V8:
  private val logger: Logger = LoggerFactory.getLogger(classOf[J2V8])

  def map2java(map: Map[String, Matchable]): java.util.Map[String, Matchable] =
    mapValues(map)(value2java).asJava

  def list2java(list: List[Matchable]): java.util.List[Matchable] =
    list.map(value2java).asJava

  private def value2java(value: Matchable): Matchable = value match
    // with value: Map[String, Matchable] I get:
    //   non-variable type argument String in type pattern scala.collection.immutable.Map[String,Matchable]
    //   (the underlying of Map[String,Matchable]) is unchecked since it is eliminated by erasure
    case value: Map[?, ?] => map2java(value.asInstanceOf[Map[String, Matchable]])
    case value: List[?] => list2java(value.asInstanceOf[List[Matchable]])
    case other => other
