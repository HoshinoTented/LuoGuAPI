package org.hoshino9.luogu.page

import com.google.gson.JsonObject
import io.ktor.client.call.receive
import io.ktor.client.request.request
import io.ktor.client.response.HttpResponse
import kotlinx.coroutines.runBlocking
import org.hoshino9.luogu.utils.HttpClient
import org.hoshino9.luogu.utils.emptyClient
import org.hoshino9.luogu.utils.json
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import java.net.URLDecoder

@Deprecated("Deprecated", ReplaceWith("AbstractLuoGuPage"))
abstract class DeprecatedLuoGuPage(open val client: HttpClient = emptyClient) : LuoGuPage {
	companion object {
		private val regex = Regex("""window\._feInjection = JSON\.parse\(decodeURIComponent\("(.+?)"\)\);""")
	}

	open val page: Document
		get() {
			return runBlocking {
				Jsoup.parse(client.request<HttpResponse>(url) {}.receive())
			}
		}

	private lateinit var _feInjection: JsonObject
	override val feInjection: JsonObject
		get() {
			return synchronized(this) {
				if (! ::_feInjection.isInitialized) refresh()

				_feInjection
			}
		}

	fun refresh() {
		_feInjection = json(URLDecoder.decode(regex.find(page.toString()) !!.groupValues[1], "UTF-8"))
	}
}