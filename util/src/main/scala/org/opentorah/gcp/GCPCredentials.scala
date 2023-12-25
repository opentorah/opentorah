package org.opentorah.gcp

import com.google.auth.oauth2.ServiceAccountCredentials

object GCPCredentials:
  def fromString(serviceAccountKey: String): ServiceAccountCredentials =
    ServiceAccountCredentials.fromStream(org.opentorah.util.Strings.string2stream(serviceAccountKey))

  def fromFile(path: String): ServiceAccountCredentials =
    ServiceAccountCredentials.fromStream(java.io.FileInputStream(path))
