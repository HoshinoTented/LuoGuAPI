package org.hoshino9.luogu.page

import cats.effect.IO
import com.google.gson.{JsonObject, JsonParser}
import org.hoshino9.luogu.LuoGuClient

trait LuoGuPageOps {
	def currentData: JsonObject

	def load(): JsonObject

	protected def refresh(): Unit
}

trait MutableLuoGuPageOps extends LuoGuPageOps {
	override def refresh(): Unit
}

trait LuoGuPage extends LuoGuPageOps {
	protected var _currentData: Option[JsonObject] = None

	override protected def refresh(): Unit = {
		_currentData = Some(load().getAsJsonObject("currentData"))
	}

	override def currentData: JsonObject = {
		_currentData match {
			case Some(data) => data
			case None => {
				refresh()
				currentData
			}
		}
	}
}

trait MutableLuoGuPage extends LuoGuPage with MutableLuoGuPageOps {
	override def refresh(): Unit = super.refresh()
}

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

trait MutableLuoGuClientPage extends LuoGuClientPage with MutableLuoGuPage