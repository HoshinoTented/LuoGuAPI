package org.hoshino9.luogu.problem.experimental

import com.google.gson.JsonObject
import okhttp3.OkHttpClient
import org.hoshino9.luogu.page.ExperimentalLuoGuPage
import org.hoshino9.luogu.problem.ProblemSearchConfig
import org.hoshino9.luogu.utils.emptyClient

open class ProblemListPage(val page: Int, val filter: ProblemSearchConfig, override val client: OkHttpClient = emptyClient) : ExperimentalLuoGuPage() {
	override val url: String get() = "https://www.luogu.org/fe/problem/list?page=$page&$filter"

	private val data: JsonObject
		get() = feInjection["currentData"]
				.asJsonObject["problems"]
				.asJsonObject

	val result: List<Problem>
		get() {
			return data["result"]
					.asJsonArray.map {
				Problem.Factory(it.asJsonObject["pid"].asString, client)
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