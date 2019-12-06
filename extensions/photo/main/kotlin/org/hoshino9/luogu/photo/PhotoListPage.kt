package org.hoshino9.luogu.photo

import com.google.gson.JsonObject
import org.hoshino9.luogu.LuoGuUtils.baseUrl
import org.hoshino9.luogu.page.AbstractLuoGuPage
import org.hoshino9.luogu.utils.HttpClient

class PhotoListPage(val page: Int, client: HttpClient) : AbstractLuoGuPage(client) {
	override val url: String get() = "$baseUrl/image?page=$page"

	private val data: JsonObject = feInjection["currentData"].asJsonObject["images"].asJsonObject

	val list: List<IPhoto>
		get() {
			return data["result"].asJsonArray.map { Photo(it.asJsonObject) }
		}

	val count: Int
		get() {
			return data["count"].asInt
		}
}