package org.hoshino9.luogu.problem

import com.google.gson.JsonObject
import okhttp3.OkHttpClient
import org.hoshino9.luogu.LuoGuUtils.baseUrl
import org.hoshino9.luogu.page.AbstractLuoGuPage
import org.hoshino9.luogu.utils.HttpClient
import org.hoshino9.luogu.utils.emptyClient

open class ProblemList(val page: Int, val filter: ProblemSearchConfig, client: HttpClient = emptyClient) : AbstractLuoGuPage(client) {
	override val url: String get() = "$baseUrl/problem/list?page=$page&$filter"

	private val data: JsonObject
		get() = feInjection["currentData"]
				.asJsonObject["problems"]
				.asJsonObject

	val result: List<IBaseProblem>
		get() {
			return data["result"]
					.asJsonArray.map {
				BaseProblem(it.asJsonObject)
			}
		}

	val count: Int
		get() {
			return data["count"].asInt
		}

	val maxPageCount: Int
		get() {
			val source = count.toDouble() / 50
			val int = source.toInt()

			return if (source == int.toDouble()) int else int + 1
		}
}