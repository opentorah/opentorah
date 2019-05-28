package org.podval.docbook.gradle.mathjax

import com.eclipsesource.v8.{NodeJS, V8}
import org.scalatest.{FlatSpec, Matchers}

class MathJaxJ2V8Test extends FlatSpec with Matchers {

  "J2V8 and Node.js" should "be available" in {
    MathJaxJ2V8.loadV8()
    val runtime: V8 = V8.createV8Runtime()
    val result: Int = runtime.executeIntegerScript(
      "var hello = 'hello, '; var world = 'world!'; hello.concat(world).length;")
    runtime.release()
    result shouldBe 13

    val nodeJS: NodeJS = NodeJS.createNodeJS()
    val version: String = nodeJS.getNodeVersion
    version.nonEmpty shouldBe true
  }
}
