package org.hoshino9.luogu.page

import com.google.gson.JsonObject
import okhttp3.OkHttpClient
import org.hoshino9.luogu.utils.*
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import java.net.URLDecoder

@Deprecated("Deprecated", ReplaceWith("AbstractLuoGuPage"))
abstract class DeprecatedLuoGuPage(open val client: OkHttpClient = emptyClient) : LuoGuPage {
	companion object {
		private val regex = Regex("""window\._feInjection = JSON\.parse\(decodeURIComponent\("(.+?)"\)\);""")
	}

	open val page: Document
		get() {
			return client.executeGet(url) {
				it.assert()
				Jsoup.parse(it.strData)
			}
		}

	override val feInjection: JsonObject
		get() {
			return json(URLDecoder.decode(regex.find(page.toString()) !!.groupValues[1], "UTF-8"))
		}
}