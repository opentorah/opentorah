package org.opentorah.calendar.service

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
    .transform[Location](fromParameter)(_.parameter)

  def fromRequest(request: Request): Location =
    fromParameter(request.url.queryParams.queryParam(queryParameterName))

  def fromParameter(parameter: Option[String]): Location =
    fromBoolean(parameter.forall(_ == "true"))

  def fromBoolean(inHolyLand: Boolean): Location =
    if inHolyLand then Location.HolyLand else Location.Diaspora
