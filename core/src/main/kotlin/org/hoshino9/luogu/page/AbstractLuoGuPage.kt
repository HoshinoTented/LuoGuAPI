package org.hoshino9.luogu.page

import com.google.gson.JsonObject
import io.ktor.client.call.receive
import kotlinx.atomicfu.update
import kotlinx.coroutines.runBlocking
import org.hoshino9.luogu.utils.HttpClient
import org.hoshino9.luogu.utils.apiGet
import org.hoshino9.luogu.utils.emptyClient
import org.hoshino9.luogu.utils.json

abstract class AbstractLuoGuPage(open val client: HttpClient = emptyClient) : BaseLuoGuPage() {
	val currentData: JsonObject get() = feInjection["currentData"].asJsonObject

	override fun refresh() {
		runBlocking {
			_feInjection.update {
				json(client.apiGet(url).receive())
			}
		}
	}
}