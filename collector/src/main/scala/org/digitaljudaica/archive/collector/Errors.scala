package org.digitaljudaica.archive.collector

// TODO monadify!
final class Errors {
  private var present: Boolean = false

  def check(): Unit = if (present) throw new IllegalArgumentException("There were errors")

  def error(message: String): Unit = {
    println(s"Error: $message")
    present = true
  }
}
