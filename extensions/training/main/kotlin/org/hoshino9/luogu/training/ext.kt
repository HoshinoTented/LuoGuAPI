@file:JvmName("TrainingUtils")

package org.hoshino9.luogu.training

import io.ktor.client.call.receive
import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.utils.HttpClient
import org.hoshino9.luogu.utils.apiPost
import org.hoshino9.luogu.utils.emptyClient

/**
 * 提升 BaseTraining 到 TrainingInfo
 */
fun BaseTraining.lift(client: HttpClient = emptyClient): TrainingInfo {
	return if (this is TrainingInfo) this else TrainingInfoPage(id).info
}

/**
 * 官方题单
 */
fun LuoGu.officialTraining(page: Int = 1): TrainingListPage {
	return TrainingListPage(page, TrainingListPage.Type.Official, client)
}

/**
 * 用户题单
 */
fun LuoGu.publicTraining(page: Int = 1): TrainingListPage {
	return TrainingListPage(page, TrainingListPage.Type.Public, client)
}

/**
 * 题单内容页面
 */
fun LuoGu.training(id: Int): TrainingInfoPage {
	return TrainingInfoPage(id, client)
}

internal fun url(id: Int, mark: Boolean) = "api/training/${if (mark) "mark" else "unmark"}/$id"

/**
 * 收藏题单
 */
suspend fun LuoGu.markTraining(id: Int) {
	apiPost(url(id, true)).receive<String>()
}

/**
 * 取消收藏题单
 */
suspend fun LuoGu.unmarkTraining(id: Int) {
	apiPost(url(id, false)).receive<String>()
}