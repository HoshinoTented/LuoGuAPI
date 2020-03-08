package org.hoshino9.luogu.page

import com.google.gson.JsonObject

interface LuoGuPage {
	companion object;

	val url: String
	val feInjection: JsonObject

	fun refresh()
}