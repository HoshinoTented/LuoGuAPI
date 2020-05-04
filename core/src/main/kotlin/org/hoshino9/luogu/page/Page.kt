package org.hoshino9.luogu.page

import com.google.gson.JsonObject
import kotlinx.coroutines.runBlocking
import org.hoshino9.luogu.LuoGuClient
import org.hoshino9.luogu.utils.parseJson

interface Page {
	val currentData: JsonObject
	suspend fun load(): JsonObject
}

interface MutablePage : Page {
	suspend fun refresh()
}

abstract class BasePage : Page {
	open val feInjection: JsonObject by lazy { runBlocking { load() } }

	override val currentData: JsonObject get() = feInjection["currentData"].asJsonObject
}

abstract class BaseMutablePage : MutablePage, BasePage() {
	private var _feInjection: JsonObject? = null

	override val feInjection: JsonObject
		get() {
			if (_feInjection == null) runBlocking { refresh() }
			return _feInjection !!
		}

	abstract val url: String
	abstract val client: LuoGuClient

	override suspend fun load(): JsonObject {
		return String(client.get(url)).parseJson().asJsonObject
	}

	override suspend fun refresh() {
		_feInjection = load()
	}
}