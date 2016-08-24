package client

import argonaut.CodecJson
import httpz.JsonToString

final case class Tag(
  id: Long,
  name: String,
  parent: Option[Long]
) extends JsonToString[Tag]

object Tag {
  implicit val tagCodecJson: CodecJson[Tag] =
    CodecJson.casecodec3(apply, unapply)(
      "id",
      "name",
      "parent_tag"
    )
}
