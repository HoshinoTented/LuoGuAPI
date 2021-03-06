package org.hoshino9.luogu.paste

import org.hoshino9.luogu.baseUrl
import org.hoshino9.luogu.page.AbstractLuoGuPage
import org.hoshino9.luogu.utils.HttpClient

class PasteList(val page: Int, client: HttpClient) : AbstractLuoGuPage(client) {
	override val url: String = "$baseUrl/paste?page=$page"

	private val data get() = feInjection["currentData"].asJsonObject["pastes"].asJsonObject

	val list: List<Paste>
		get() {
			return data["result"].asJsonArray.map {
				PasteImpl(it.asJsonObject)
			}
		}

	val count: Int
		get() {
			return data["count"].asInt
		}
}