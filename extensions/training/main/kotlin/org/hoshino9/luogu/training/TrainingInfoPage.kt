package org.hoshino9.luogu.training

import com.google.gson.JsonObject
import org.hoshino9.luogu.baseUrl
import org.hoshino9.luogu.page.AbstractLuoGuPage
import org.hoshino9.luogu.page.currentData
import org.hoshino9.luogu.utils.HttpClient
import org.hoshino9.luogu.utils.delegate
import org.hoshino9.luogu.utils.emptyClient

class TrainingInfoPage(val id: Int, client: HttpClient = emptyClient) : AbstractLuoGuPage(client) {
	override val url: String = "$baseUrl/training/$id"

	val data = currentData.delegate
	private val training: JsonObject by data
	val canEdit: Boolean by data

	val info: TrainingInfo by lazy {
		TrainingInfoImpl(training)
	}
}