package org.hoshino9.luogu.page

import com.google.gson.JsonObject

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