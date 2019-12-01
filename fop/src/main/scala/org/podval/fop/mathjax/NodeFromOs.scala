package org.podval.fop.mathjax

import java.io.File

import org.podval.fop.util.{Os, Platform}

import scala.sys.process.Process

object NodeFromOs {

  def apply(nodeModulesParent: File): Option[Node] = {
    if (Platform.getOs == Os.Windows) None else {
      def which(what: String): Option[File] = try {
        Some(new File(Process(s"which $what").!!.trim))
      } catch {
        case e: Exception => None
      }

      for {
        nodeExec <- which("node")
        npmExec <- which("npm")
      } yield new Node(
        nodeModulesParent,
        nodeExec,
        npmExec
      )
    }
  }
}
