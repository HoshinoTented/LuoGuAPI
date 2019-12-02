package org.hoshino9.luogu.paste

import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.page.AbstractLuoGuPage
import org.hoshino9.luogu.utils.HttpClient

class PasteList(client: HttpClient) : AbstractLuoGuPage(client) {
	override val url: String = "https://www.luogu.org/paste"

	private val data get() = feInjection["currentData"].asJsonObject["pastes"].asJsonObject["result"].asJsonArray

	val list: List<Paste>
		get() {
			return data.map {
				Paste.Factory(it.asJsonObject).newInstance()
			}
		}
}