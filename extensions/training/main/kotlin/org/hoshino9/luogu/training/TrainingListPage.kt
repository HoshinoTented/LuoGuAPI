package org.hoshino9.luogu.training

import com.google.gson.JsonArray
import org.hoshino9.luogu.LuoGuUtils.baseUrl
import org.hoshino9.luogu.page.AbstractLuoGuPage
import org.hoshino9.luogu.utils.HttpClient
import org.hoshino9.luogu.utils.delegate
import org.hoshino9.luogu.utils.emptyClient

class TrainingListPage(val page: Int = 1, val type: Type, client: HttpClient = emptyClient) : AbstractLuoGuPage() {
	enum class Type {
		Official,
		Public
	}

	override val url: String = "$baseUrl/training/list?type=${type.name.toLowerCase()}&page=$page"

	val delegate = currentData["trainings"].asJsonObject.delegate
	val count: Int by delegate
	private val result: JsonArray by delegate

	val trainings: List<BaseTraining> by lazy {
		result.map {
			BaseTrainingImpl(it)
		}
	}
}