package org.podval.docbook.gradle.mathjax

import java.io.File
import java.nio.file.Files

import com.eclipsesource.v8.{NodeJS, V8, V8Array, V8Function, V8Object}
import com.eclipsesource.v8.utils.V8ObjectUtils

import scala.collection.JavaConverters._

final class MathJaxJ2V8(nodeModulesRoot: File) {

  private def v8: V8 = nodeJS.getRuntime

  private val nodeJS: NodeJS = {
    MathJaxJ2V8.loadV8()
    NodeJS.createNodeJS()
  }

  private val mathJaxNode: V8Object =
    nodeJS.require(new File(nodeModulesRoot, "node_modules/mathjax-node"))

  def configure(configurationMap: Map[String, Any]): Unit = {
    val args: V8Array = V8ObjectUtils.toV8Array(v8, List(MathJaxJ2V8.map2java(configurationMap)).asJava)
    mathJaxNode.executeVoidFunction("config", args)
    args.release()
  }

  // This is done automatically when typeset is first called.
  //  private def start(): Unit = {
  //    val args = new V8Array(v8)
  //    mathJaxNode.executeVoidFunction("start", args)
  //    args.release()
  //  }

  def close() {
    mathJaxNode.release()
    nodeJS.release()
  }

  def typeset(optionsMap: Map[String, Any], outputName: String): String = {
    val data: V8Object = typeset(optionsMap)
    val result: String = data.getString(outputName)
    data.release()
    result
  }

  //  def typeset2V8Object(options: Map[String, Any], outputName: String): V8Object = {
  //    val data: V8Object = typeset(options)
  //    val result: V8Object = data.getObject(outputName)
  //    data.release()
  //    result
  //  }

  private def typeset(optionsMap: Map[String, Any]): V8Object = {
    var result: V8Object = null

    val callback: V8Function = new V8Function(v8, (_ /*receiver*/: V8Object, parameters: V8Array) => {
      result = parameters.getObject(0)
      val errors: V8Array = result.getArray("errors")
      if (!errors.isUndefined)
        throw new IllegalArgumentException(V8ObjectUtils.toList(errors).asScala.map(_.asInstanceOf[String]).mkString("\n"))
      errors.release()
      null
    })

    val args: V8Array = V8ObjectUtils.toV8Array(v8, MathJaxJ2V8.list2java(List(optionsMap, callback)))
    val callResult: V8Object = mathJaxNode.executeObjectFunction("typeset", args)

    while (nodeJS.isRunning) nodeJS.handleMessage()

    callback.release()
    args.release()
    callResult.release()

    result
  }
}

object MathJaxJ2V8 {

  def loadV8(): Unit = {
    if (!V8.isLoaded) V8.createV8Runtime(
      "dummy",
      Files.createTempDirectory("j2v8").toAbsolutePath.toString
    ).release()
  }

  private def map2java(map: Map[String, Any]): java.util.Map[String, Any] =
    map.mapValues(value2java).asJava

  private def list2java(list: List[Any]): java.util.List[Any] =
    list.map(value2java).asJava

  private def value2java(value: Any): Any = value match {
    case value: Map[String, Any] => map2java(value)
    case value: List[Any] => list2java(value)
    case other => other
  }

  // private def j2v8Version: String = "4.8.0"

  // def j2v8dependency: String = s"com.eclipsesource.j2v8:j2v8_${getOSName(???)}_$getArchSuffix:$j2v8Version"

//  private def getOSname(os: Os): String = os match {
//    case Os.Windows => "win32"
//    case Os.Mac => "macosx"
//    case Os.Linux => "linux"
//    case Os.Android => "android"
//    case _ => throw new UnsatisfiedLinkError(s"Unsupported platform: $os")
//  }

//  private def getArchSuffix(architecture: Architecture): String = architecture match {
//    case Architecture.i686 => "x86"
//    case Architecture.amd64 => "x86_64"
//    case Architecture.nacl => "armv7l"
//    case Architecture.aarch64 => "armv7l"
//    case _ => throw new UnsatisfiedLinkError(s"Unsupported architecture: $architecture")
//  }
}
