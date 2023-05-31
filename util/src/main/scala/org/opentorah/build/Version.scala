package org.opentorah.build

import scala.CanEqual

// TODO verify syntax
final class Version(val version: String) derives CanEqual:
  override def toString: String = version

  override def equals(other: Any): Boolean = other match
    case that: Version => this.version == that.version
    case _ => false

  def major: Int = version.split('.')(0).toInt
    
  def getMajorMinorMicro: (Int, Int, Int) =
    val versionTokens: Array[String] = version.split('.')
    (
      versionTokens(0).toInt,
      versionTokens(1).toInt,
      versionTokens(2).toInt
    )
    
  def majorAndMinor: String =
    version.split('.').take(2).mkString(".")  
