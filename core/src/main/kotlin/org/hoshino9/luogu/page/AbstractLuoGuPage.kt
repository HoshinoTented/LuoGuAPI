package org.hoshino9.luogu.page

import com.google.gson.JsonObject
import io.ktor.client.call.receive
import kotlinx.atomicfu.update
import kotlinx.coroutines.runBlocking
import org.hoshino9.luogu.LuoGuClient
import org.hoshino9.luogu.utils.HttpClient
import org.hoshino9.luogu.utils.apiGet
import org.hoshino9.luogu.utils.emptyClient
import org.hoshino9.luogu.utils.json

/**
 * 一般抽象洛谷页面抽象类，几乎所有的洛谷页面都需要继承这个抽象类。
 * 提供了 [refresh] 的默认实现。
 */
abstract class AbstractLuoGuPage(val client: HttpClient = emptyClient) : BaseLuoGuPage() {
	override fun refresh() {
		runBlocking {
			_feInjection.update {
				json(client.apiGet(url).receive())
			}
		}
	}
}

abstract class AbstractLuoGuClientPage(val client: LuoGuClient) : BaseLuoGuPage() {
	override fun refresh() {
		runBlocking {
			_feInjection.update {
				json(String(client.get(url)))
			}
		}
	}
}