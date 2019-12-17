package org.hoshino9.luogu.page

import com.google.gson.JsonObject
import io.ktor.client.call.receive
import io.ktor.client.features.ClientRequestException
import kotlinx.coroutines.runBlocking
import org.hoshino9.luogu.utils.*

abstract class AbstractLuoGuPage(open val client: HttpClient = emptyClient) : LuoGuPage {

	/**
	 * 获取页面数据
	 *
	 * **注意，这个是 Lazy 的**
	 *
	 * @throws ClientRequestException
	 */
	override val feInjection: JsonObject by lazy {
		runBlocking {
			json(client.apiGet(url).receive())
		}
	}

	val currentData: JsonObject get() = feInjection["currentData"].asJsonObject
}