package org.podval.fop.mathjax

import java.io.File

import com.eclipsesource.v8.utils.V8ObjectUtils
import com.eclipsesource.v8._
import org.podval.fop.util.Logger

import scala.jdk.CollectionConverters._

// NOTE: some tests failed unless I typeset specific TeX math first; some - even then;
// re-configuring and forcibly re-starting MathJax before each typeset call breaks the tests even more;
// sometimes, stopping Gradle daemon helped; once, JVM crashed; once, I got:
//   Invalid V8 thread access: current thread is Thread[Execution worker for ':',5,main]
//   while the locker has thread Thread[Execution worker for ':',5,]
// Conclusion: MathJax has to be created, used and closed by the same thread - duh!
// For now, I just create, use and dispose a fresh MathJax instance for every typesetting -
// but should probably do the worker thing mentioned in the J2V8 documentation.
//
// Some tests fail if their order is reversed when mathJax instance is re-used;
// this doesn't look like a threading issue - or maybe whatever I "solved" by using fresh MathJax instance
// for each typesetting wasn't (just) a threading issue either?
final class J2V8MathJax(
  node: Node,
  configuration: Configuration,
  logger: Logger
) extends MathJax(node, configuration, logger) {
  override def typeset(
    options: Map[String, Any],
    outputName: String
  ): String = {
    val worker: J2V8MathJaxWorker = new J2V8MathJaxWorker(node.nodeModules)

    worker.configure(configuration.toMap)
    val result = worker.typeset(options, outputName)
    worker.close()
    result
  }
}

private final class J2V8MathJaxWorker(nodeModules: File) {
  val nodeJS: NodeJS = NodeJS.createNodeJS

  def v8: V8 = nodeJS.getRuntime

  val mathJaxNode: V8Object =
    nodeJS.require(new File(nodeModules, "mathjax-node"))

  def configure(configuration: Map[String, Any]): Unit = {
    val args: V8Array = V8ObjectUtils.toV8Array(v8, List(J2V8.map2java(configuration)).asJava)
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

    val args: V8Array = V8ObjectUtils.toV8Array(v8, J2V8.list2java(List(optionsMap, callback)))
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
