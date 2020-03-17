@file:JvmName("TrainingUtils")

package org.hoshino9.luogu.training

import io.ktor.client.call.receive
import io.ktor.client.request.get
import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.baseUrl
import org.hoshino9.luogu.user.UserPage
import org.hoshino9.luogu.utils.*

/**
 * 提升 BaseTraining 到 TrainingInfo
 */
fun BaseTraining.lift(client: HttpClient = emptyClient): TrainingInfo {
	return if (this is TrainingInfo) this else TrainingInfoPage(id, client).info
}

/**
 * 官方题单
 */
fun LuoGu.officialTraining(page: Int = 1): TrainingListPage {
	return TrainingListPageBuilder(page, TrainingListPageBuilder.Type.Official, client).build()
}

/**
 * 用户题单
 */
fun LuoGu.publicTraining(page: Int = 1): TrainingListPage {
	return TrainingListPageBuilder(page, TrainingListPageBuilder.Type.Select, client).build()
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
suspend fun LuoGu.markTraining(id: Int): String {
	return apiPost(url(id, true)).receive()
}

/**
 * 取消收藏题单
 */
suspend fun LuoGu.unmarkTraining(id: Int): String {
	return apiPost(url(id, false)).receive()
}

suspend fun UserPage.trainingList(page: Int = 1): TrainingListPage {
	val url = "$baseUrl/fe/api/user/createdTrainings?page=$page"
	val data = json(client.get(url))

	return TrainingListPageImpl(data)
}