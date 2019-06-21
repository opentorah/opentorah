package org.podval.docbook.gradle.j2v8

import org.podval.docbook.gradle.mathjax.{Configuration, Typesetter}
import org.podval.docbook.gradle.node.Installation
import org.podval.docbook.gradle.util.Logger

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
object OneUseMathJaxTypesetterFactory extends Typesetter.Factory {

  override def get(installation: Installation, configuration: Configuration, logger: Logger): Typesetter =
    new Typesetter(configuration, logger) {
      override protected def typeset(options: Map[String, Any], outputName: String): String = {
        val mathJax = new MathJax(installation.nodeModules)
        mathJax.configure(configuration)
        val result = mathJax.typeset(options, outputName)
        mathJax.close()
        result
      }
    }
}
