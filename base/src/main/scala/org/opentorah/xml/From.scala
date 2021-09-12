package org.opentorah.xml

import java.io.File
import java.net.URL
import org.opentorah.util.{Effects, Files, Util}
import zio.{IO, Task}

sealed abstract class From(val name: String):

  def url: Option[URL]

  def load: IO[Effects.Error, ScalaXml.Element]

  final def loadTask: Task[ScalaXml.Element] = Effects.error2throwable(load)

  def isRedirect: Boolean

object From:

  private final class FromXml(
    name: String,
    elem: ScalaXml.Element
  ) extends From(name):
    override def isRedirect: Boolean = false
    override def toString: String = s"From.xml($name)"
    override def url: Option[URL] = None
    override def load: IO[Effects.Error, ScalaXml.Element] = IO.succeed(elem)

  def xml(name: String, elem: ScalaXml.Element): From = new FromXml(name, elem)

  private final class FromString(
    name: String,
    string: String
  ) extends From(name):
    override def isRedirect: Boolean = false
    override def toString: String = s"From.string($name)"
    override def url: Option[URL] = None
    override def load: IO[Effects.Error, ScalaXml.Element] = Effects.effect(ScalaXml.loadFromString(string))

  def string(name: String, string: String): From = new FromString(name, string)

  private final class FromUrl(fromUrl: URL, override val isRedirect: Boolean)
    extends From(Files.nameAndExtension(fromUrl.getPath)._1):
    override def toString: String = s"From.url($fromUrl, isRedirect=$isRedirect)"
    override def url: Option[URL] = Some(fromUrl)
    override def load: IO[Effects.Error, ScalaXml.Element] = Effects.effect(ScalaXml.loadFromUrl(fromUrl))

  def url(url: URL): From = new FromUrl(url, false)

  private[xml] def redirect(url: URL): From = new FromUrl(url, true)

  def file(directory: File, fileName: String): From = file(File(directory, fileName + ".xml"))
  def file(file: File): From = url(Files.file2url(file))

  private final class FromResource(
    clazz: Class[?],
    name: String
  ) extends From(name):
    override def isRedirect: Boolean = false
    override def toString: String = s"From.resource($clazz:$name.xml)"
    override def url: Option[URL] = Option(clazz.getResource(name + ".xml"))
    override def load: IO[Effects.Error, ScalaXml.Element] = url
      .map(url => Effects.effect(ScalaXml.loadFromUrl(url)))
      .getOrElse(Effects.fail(s"Resource not found: $this"))

  def resource(obj: AnyRef, name: String): From = new FromResource(obj.getClass, name)
  def resource(obj: AnyRef): From = resource(obj, Util.className(obj))
