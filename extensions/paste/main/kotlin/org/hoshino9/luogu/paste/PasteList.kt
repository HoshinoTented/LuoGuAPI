package org.hoshino9.luogu.paste

import org.hoshino9.luogu.LuoGuUtils.baseUrl
import org.hoshino9.luogu.page.AbstractLuoGuPage
import org.hoshino9.luogu.utils.HttpClient

class PasteList(client: HttpClient) : AbstractLuoGuPage(client) {
	override val url: String = "$baseUrl/paste"

	private val data get() = feInjection["currentData"].asJsonObject["pastes"].asJsonObject["result"].asJsonArray

	val list: List<IPaste>
		get() {
			return data.map {
				Paste(it.asJsonObject)
			}
		}
}