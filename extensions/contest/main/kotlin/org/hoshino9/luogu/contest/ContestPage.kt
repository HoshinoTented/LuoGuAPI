package org.hoshino9.luogu.contest

import com.google.gson.JsonObject
import org.hoshino9.luogu.LuoGuUtils.baseUrl
import org.hoshino9.luogu.page.AbstractLuoGuPage
import org.hoshino9.luogu.utils.HttpClient
import org.hoshino9.luogu.utils.provider
import org.hoshino9.luogu.utils.emptyClient

class ContestPage(val id: Int, client: HttpClient = emptyClient) : AbstractLuoGuPage(client) {
	data class Problem(val id: String, val score: Int, val submitted: Boolean)

	override val url: String get() = "$baseUrl/contest/$id"

	private val data = feInjection["currentData"].asJsonObject
	private val provider = data.provider

	val contest: IContest
		get() {
			return Contest(data["contest"].asJsonObject)
		}

	val accessLevel: Int by provider.provide()
	val joined: Boolean by provider.provide()

	val contestProblems: List<Problem>?
		get() {
			return data["contestProblems"].let { problems ->
				if (problems.isJsonNull) null else {
					problems.asJsonArray.map { problem ->
						problem.asJsonObject.provider.let {
							val score: Int by it.provide()
							val submitted: Boolean by it.provide()

							Problem(problem.asJsonObject["problem"].asJsonObject["pid"].asString, score, submitted)
						}
					}
				}
			}
		}
}