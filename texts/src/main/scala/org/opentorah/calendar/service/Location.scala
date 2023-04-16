package org.opentorah.calendar.service

import org.opentorah.service.ServiceApp
import zio.http.Request
import zio.http.codec.{HttpCodec, HttpCodecType}

enum Location(val name: String, val inHolyLand: Boolean) derives CanEqual:
  def parameter: Option[String] = if !inHolyLand then Some(false.toString) else None
  case HolyLand extends Location("Holy Land", true)
  case Diaspora extends Location("Diaspora", false)

object Location:
  val queryParameterName: String = "inHolyLand"

  val codec: HttpCodec[HttpCodecType.Query, Location] = HttpCodec
    .query(queryParameterName)
    .optional
    .transform[Location](
      get,
      _.parameter
    )

  def get(request: Request): Location =
    get(ServiceApp.queryParameter(request, queryParameterName))

  def get(parameter: Option[String]): Location =
    get(parameter.forall(_ == "true"))

  def get(inHolyLand: Boolean): Location =
    if inHolyLand then Location.HolyLand else Location.Diaspora
