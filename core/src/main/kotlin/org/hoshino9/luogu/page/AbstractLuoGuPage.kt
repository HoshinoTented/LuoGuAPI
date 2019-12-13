package org.hoshino9.luogu.page

import com.google.gson.JsonObject
import io.ktor.client.call.receive
import kotlinx.coroutines.runBlocking
import okhttp3.Headers
import okhttp3.OkHttpClient
import okhttp3.Response
import org.hoshino9.luogu.utils.*

abstract class AbstractLuoGuPage(open val client: HttpClient = emptyClient) : LuoGuPage {
	override val feInjection: JsonObject
		get() = runBlocking {
			json(client.apiGet(url).receive())
		}

	val currentData: JsonObject get() = feInjection["currentData"].asJsonObject
}