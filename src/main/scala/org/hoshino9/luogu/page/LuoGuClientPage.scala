package org.hoshino9.luogu.page

import com.google.gson.{JsonObject, JsonParser}
import org.hoshino9.luogu.LuoGuClient

trait LuoGuClientPage extends LuoGuPage {
	val url: String
	val client: LuoGuClient

	def content: String = {
		client.get(url).body().string
	}

	override def load(): JsonObject = {
		JsonParser.parseString(content).getAsJsonObject
	}
}
