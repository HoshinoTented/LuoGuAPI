package org.hoshino9.luogu.page

import com.google.gson.JsonObject

trait LuoGuPageOps {
	def currentData: JsonObject

	def load(): JsonObject

	protected def refresh(): Unit
}
