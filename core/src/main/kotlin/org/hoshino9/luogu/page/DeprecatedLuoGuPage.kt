package org.hoshino9.luogu.page

import io.ktor.client.call.receive
import io.ktor.client.request.request
import io.ktor.client.response.HttpResponse
import kotlinx.atomicfu.update
import kotlinx.coroutines.runBlocking
import org.hoshino9.luogu.utils.HttpClient
import org.hoshino9.luogu.utils.emptyClient
import org.hoshino9.luogu.utils.json
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import java.net.URLDecoder

/**
 * 过时洛谷页面抽象类，在非必要情况下不应该使用。
 * 提供了 [refresh] 的默认实现。
 */
@Deprecated("Deprecated", ReplaceWith("AbstractLuoGuPage"))
abstract class DeprecatedLuoGuPage(open val client: HttpClient = emptyClient) : BaseLuoGuPage() {
	companion object {
		private val regex = Regex("""window\._feInjection = JSON\.parse\(decodeURIComponent\("(.+?)"\)\);""")
	}

	open suspend fun page(): Document {
		return Jsoup.parse(client.request<HttpResponse>(url) {}.receive())
	}

	override fun refresh() {
		runBlocking {
			_feInjection.update {
				json(URLDecoder.decode(regex.find(page().toString()) !!.groupValues[1], "UTF-8"))
			}
		}
	}
}