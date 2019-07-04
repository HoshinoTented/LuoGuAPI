package org.hoshino9.luogu.page

import com.google.gson.JsonObject
import okhttp3.Headers
import okhttp3.OkHttpClient
import okhttp3.Response
import org.hoshino9.luogu.utils.*

abstract class ExperimentalLuoGuPage(open val client: OkHttpClient = emptyClient) : LuoGuPage {
	override val feInjection: JsonObject
		get() {
			return client.contentOnlyGet(url) {
				it.assert()
				json(it.strData)
			}
		}

	companion object {
		inline fun <T> HttpClient.contentOnlyGet(url: String, action: (Response) -> T): T {
			return executeGet(url, Headers.of("x-luogu-type", "content-only"), action)
		}
	}
}