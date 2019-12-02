package org.hoshino9.luogu.page

import com.google.gson.JsonObject

interface LuoGuPage {
	val url: String
	val feInjection: JsonObject
}