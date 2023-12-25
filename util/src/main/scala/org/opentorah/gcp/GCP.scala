package org.opentorah.gcp

object GCP:
  // This is set when running in Cloud Run (or KNative in general?)
  def getServiceName: Option[String] = Option(System.getenv("K_SERVICE"))
