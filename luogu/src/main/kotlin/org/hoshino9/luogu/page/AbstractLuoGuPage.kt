package org.hoshino9.luogu.page

import com.google.gson.JsonObject
import okhttp3.Headers
import okhttp3.OkHttpClient
import okhttp3.Response
import org.hoshino9.luogu.utils.*

abstract class AbstractLuoGuPage(open val client: OkHttpClient = emptyClient) : LuoGuPage {
	override val feInjection: JsonObject
		get() = client.apiGet(url)

	val currentData: JsonObject get() = feInjection["currentData"].asJsonObject
}