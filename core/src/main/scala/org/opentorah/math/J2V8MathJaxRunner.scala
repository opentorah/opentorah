package org.opentorah.math

import com.eclipsesource.v8.*
import com.eclipsesource.v8.utils.V8ObjectUtils
import java.io.File
import scala.jdk.CollectionConverters.*

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
final class J2V8MathJaxRunner(
  node: Node,
  math: MathConfiguration
) extends MathJaxRunner(
  math
):

  private def mathJax: MathJax = math.mathJax

  override def typeset(
    options: Map[String, Matchable],
    outputName: String
  ): String =
    val nodeJS: NodeJS = NodeJS.createNodeJS
    def v8: V8 = nodeJS.getRuntime
    val mathJaxNode: V8Object = nodeJS.require(File(node.nodeModules, mathJax.packageName))

    val configurationArgs: V8Array =
      V8ObjectUtils.toV8Array(v8, List(J2V8.map2java(mathJax.nodeConfiguration(math))).asJava)
    mathJaxNode.executeVoidFunction(mathJax.configurationFunction, configurationArgs)
    configurationArgs.release()

    var data: V8Object = null

    val callback: V8Function = V8Function(v8, (_ /*receiver*/: V8Object, parameters: V8Array) => {
      data = parameters.getObject(0)
      val errors: V8Array = data.getArray(mathJax.errorsArray)
      if !errors.isUndefined then
        throw IllegalArgumentException(V8ObjectUtils.toList(errors).asScala.map(_.asInstanceOf[String]).mkString("\n"))
      errors.release()
      null
    })

    val typesetArgs: V8Array = V8ObjectUtils.toV8Array(v8, J2V8.list2java(List(options, callback)))

    // This is done automatically when typeset is first called.
    //  private def start(): Unit = {
    //    val args = new V8Array(v8)
    //    mathJaxNode.executeVoidFunction("start", args)
    //    args.release()
    //  }
    val callResult: V8Object = mathJaxNode.executeObjectFunction(mathJax.typesetFunction, typesetArgs)

    while nodeJS.isRunning do nodeJS.handleMessage()

    callback.release()
    typesetArgs.release()
    callResult.release()

    val result: String = data.getString(outputName)
    data.release()

    mathJaxNode.release()
    nodeJS.release()

    result
