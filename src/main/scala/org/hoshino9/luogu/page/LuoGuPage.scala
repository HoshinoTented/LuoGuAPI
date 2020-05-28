package org.hoshino9.luogu.page

import play.api.libs.json.{JsObject, JsValue}

trait LuoGuPage extends LuoGuPageOps {
	protected var _currentData: Option[JsObject] = None

	override protected def refresh(): Unit = {
		_currentData = Some(load()("currentData").as[JsObject])
	}

	override def currentData: JsObject = {
		_currentData match {
			case Some(data) => data
			case None => {
				refresh()
				currentData
			}
		}
	}
}