package org.hoshino9.luogu.contest

import org.hoshino9.luogu.baseUrl
import org.hoshino9.luogu.page.AbstractLuoGuPage
import org.hoshino9.luogu.utils.HttpClient
import org.hoshino9.luogu.utils.emptyClient

class ContestListPage(val page: Int = 1, client: HttpClient = emptyClient) : AbstractLuoGuPage(client) {
	override val url: String
		get() = "$baseUrl/contest/list?page=$page"

	private val data = feInjection["currentData"].asJsonObject["contests"].asJsonObject

	val count: Int
		get() = data["count"].asInt


	val contests: List<BaseContest>
		get() {
			return data["result"].asJsonArray.map {
				BaseContestImpl(it)
			}
		}
}