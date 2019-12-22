package org.hoshino9.luogu.page

import com.google.gson.JsonObject
import io.ktor.client.call.receive
import io.ktor.client.features.ClientRequestException
import kotlinx.coroutines.runBlocking
import org.hoshino9.luogu.utils.*

abstract class AbstractLuoGuPage(open val client: HttpClient = emptyClient) : BaseLuoGuPage() {
	val currentData: JsonObject get() = feInjection["currentData"].asJsonObject

	override fun refresh() {
		_feInjection = runBlocking {
			json(client.apiGet(url).receive())
		}
	}
}