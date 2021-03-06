package org.hoshino9.luogu.contest

import org.hoshino9.luogu.baseUrl
import org.hoshino9.luogu.page.AbstractLuoGuPage
import org.hoshino9.luogu.utils.HttpClient
import org.hoshino9.luogu.utils.delegate
import org.hoshino9.luogu.utils.emptyClient

class ContestPage(val id: Int, client: HttpClient = emptyClient) : AbstractLuoGuPage(client) {
	data class Problem(val id: String, val score: Int, val submitted: Boolean)

	override val url: String get() = "$baseUrl/contest/$id"

	private val data = feInjection["currentData"].asJsonObject
	private val delegate = data.delegate

	val contest: Contest
		get() {
			return ContestImpl(data["contest"].asJsonObject)
		}

	val accessLevel: Int by delegate
	val joined: Boolean by delegate

	val contestProblems: List<Problem>?
		get() {
			return data["contestProblems"].let { problems ->
				if (problems.isJsonNull) null else {
					problems.asJsonArray.map { problem ->
						problem.asJsonObject.delegate.let {
							val score: Int by it
							val submitted: Boolean by it

							Problem(problem.asJsonObject["problem"].asJsonObject["pid"].asString, score, submitted)
						}
					}
				}
			}
		}
}