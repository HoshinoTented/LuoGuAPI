@file:JvmName("ContestUtils")

package org.hoshino9.luogu.contest

import io.ktor.client.call.receive
import io.ktor.client.features.ClientRequestException
import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.utils.apiPost
import org.hoshino9.luogu.utils.referer

/**
 * 获取比赛列表页面
 *
 * @param page 页码
 */
fun LuoGu.contestListPage(page: Int = 1): ContestListPage {
	return ContestListPage(page, client)
}

/**
 * 获取比赛详细页面
 *
 * @param id 比赛 ID
 */
fun LuoGu.contestPage(id: Int): ContestPage {
	return ContestPage(id, client)
}

/**
 * 加入比赛
 *
 * @param id 比赛 ID
 *
 * @throws ClientRequestException
 */
suspend fun LuoGu.joinContest(id: Int) {
	apiPost("fe/api/contest/join/$id") {
		referer("contest/$id")
	}.receive<String>()
}