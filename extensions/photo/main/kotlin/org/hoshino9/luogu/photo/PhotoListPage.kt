package org.hoshino9.luogu.photo

import com.google.gson.JsonObject
import org.hoshino9.luogu.LuoGuUtils.baseUrl
import org.hoshino9.luogu.page.AbstractLuoGuPage
import org.hoshino9.luogu.utils.HttpClient
import org.hoshino9.luogu.utils.delegate

class PhotoListPage(val page: Int, client: HttpClient) : AbstractLuoGuPage(client) {
	override val url: String get() = "$baseUrl/image?page=$page"

	private val data: JsonObject = feInjection["currentData"].asJsonObject
	private val images: JsonObject = data["images"].asJsonObject
	private val dataDelegate = data.delegate
	private val imagesDelegate = images.delegate

	val spaceUsage: Int by dataDelegate
	val spaceLimit: Int by dataDelegate
	val count: Int by imagesDelegate

	val list: List<IPhoto>
		get() {
			return images["result"].asJsonArray.map { Photo(it.asJsonObject) }
		}
}