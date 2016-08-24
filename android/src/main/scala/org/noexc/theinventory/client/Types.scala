package client

import argonaut.CodecJson
import httpz.JsonToString

final case class Tag(
  id: Long,
  name: String,
  parent: Option[Long],
  creationTime: String // TODO: Better type here
) extends JsonToString[Tag]

object Tag {
  implicit val tagCodecJson: CodecJson[Tag] =
    CodecJson.casecodec4(apply, unapply)(
      "id",
      "name",
      "parent_tag",
      "creation_time"
    )
}
