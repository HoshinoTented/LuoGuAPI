package org.hoshino9.luogu.page

import okhttp3.Headers
import okhttp3.OkHttpClient
import org.hoshino9.luogu.utils.*
import org.json.JSONObject

abstract class ExperimentalLuoGuPage(open val client: OkHttpClient = emptyClient) : LuoGuPage {
	override val feInjection: JSONObject
		get() {
			return client.executeGet(url, Headers.of("x-luogu-type", "content-only")) {
				it.assert()
				json(it.strData)
			}
		}
}