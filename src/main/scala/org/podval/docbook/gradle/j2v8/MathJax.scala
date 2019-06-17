package org.podval.docbook.gradle.j2v8

import java.io.File

import com.eclipsesource.v8.utils.V8ObjectUtils
import com.eclipsesource.v8.{NodeJS, V8, V8Array, V8Function, V8Object}
import org.podval.docbook.gradle.mathjax.Configuration

import scala.jdk.CollectionConverters._

final class MathJax(nodeModulesRoot: File) {

  private val nodeJS: NodeJS = NodeJS.createNodeJS()

  private def v8: V8 = nodeJS.getRuntime

  private val mathJaxNode: V8Object =
    nodeJS.require(new File(nodeModulesRoot, "node_modules/mathjax-node"))

  def configure(configuration: Configuration): Unit = {
    val args: V8Array = V8ObjectUtils.toV8Array(v8, List(MathJax.map2java(configuration.toMap)).asJava)
    mathJaxNode.executeVoidFunction("config", args)
    args.release()
  }

  // This is done automatically when typeset is first called.
  //  private def start(): Unit = {
  //    val args = new V8Array(v8)
  //    mathJaxNode.executeVoidFunction("start", args)
  //    args.release()
  //  }

  def typeset(optionsMap: Map[String, Any], outputName: String): String = {
    var data: V8Object = null

    val callback: V8Function = new V8Function(v8, (_ /*receiver*/: V8Object, parameters: V8Array) => {
      data = parameters.getObject(0)
      val errors: V8Array = data.getArray("errors")
      if (!errors.isUndefined)
        throw new IllegalArgumentException(V8ObjectUtils.toList(errors).asScala.map(_.asInstanceOf[String]).mkString("\n"))
      errors.release()
      null
    })

    val args: V8Array = V8ObjectUtils.toV8Array(v8, MathJax.list2java(List(optionsMap, callback)))
    val callResult: V8Object = mathJaxNode.executeObjectFunction("typeset", args)

    while (nodeJS.isRunning) nodeJS.handleMessage()

    callback.release()
    args.release()
    callResult.release()

    val result: String = data.getString(outputName)
    data.release()
    result
  }

  def close(): Unit = {
    mathJaxNode.release()
    nodeJS.release()
  }
}

object MathJax {

  private def map2java(map: Map[String, Any]): java.util.Map[String, Any] =
    map.view.mapValues(value2java).toMap.asJava

  private def list2java(list: List[Any]): java.util.List[Any] =
    list.map(value2java).asJava

  private def value2java(value: Any): Any = value match {
    case value: Map[String, Any] => map2java(value)
    case value: List[Any] => list2java(value)
    case other => other
  }
}
