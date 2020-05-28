package org.hoshino9.luogu.page
import org.hoshino9.luogu.LuoGuClient
import play.api.libs.json.{JsObject, Json}

trait LuoGuClientPage extends LuoGuPage {
	val url: String
	val client: LuoGuClient

	def content: String = {
		client.get(url).body().string
	}

	override def load(): JsObject = {
		Json.parse(content).asInstanceOf[JsObject]
	}
}
