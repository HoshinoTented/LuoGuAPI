package org.hoshino9.luogu.page

import play.api.libs.json.JsObject

trait LuoGuPageOps {
	def currentData: JsObject

	def load(): JsObject

	protected def refresh(): Unit
}
