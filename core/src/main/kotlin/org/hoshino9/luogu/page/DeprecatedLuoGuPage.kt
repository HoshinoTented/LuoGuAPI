package org.hoshino9.luogu.page

import com.google.gson.JsonObject
import io.ktor.client.call.call
import io.ktor.client.call.receive
import kotlinx.coroutines.runBlocking
import okhttp3.OkHttpClient
import org.hoshino9.luogu.utils.*
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
				Jsoup.parse(client.call(url).receive())
			}
		}

	override val feInjection: JsonObject
		get() {
			return json(URLDecoder.decode(regex.find(page.toString()) !!.groupValues[1], "UTF-8"))
		}
}