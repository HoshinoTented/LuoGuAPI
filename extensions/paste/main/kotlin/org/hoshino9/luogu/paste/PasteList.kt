package org.hoshino9.luogu.paste

import org.hoshino9.luogu.LuoGuUtils.baseUrl
import org.hoshino9.luogu.page.AbstractLuoGuPage
import org.hoshino9.luogu.utils.HttpClient

class PasteList(val page: Int, client: HttpClient) : AbstractLuoGuPage(client) {
	override val url: String = "$baseUrl/paste?page=$page"

	private val data get() = feInjection["currentData"].asJsonObject["pastes"].asJsonObject

	val list: List<IPaste>
		get() {
			return data["result"].asJsonArray.map {
				Paste(it.asJsonObject)
			}
		}

	val count: Int
		get() {
			return data["count"].asInt
		}
}