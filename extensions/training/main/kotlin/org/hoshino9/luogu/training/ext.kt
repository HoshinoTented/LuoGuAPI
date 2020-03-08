@file:JvmName("TrainingUtils")

package org.hoshino9.luogu.training

import io.ktor.client.call.receive
import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.utils.HttpClient
import org.hoshino9.luogu.utils.apiPost
import org.hoshino9.luogu.utils.emptyClient

fun BaseTraining.lift(client: HttpClient = emptyClient): TrainingInfo {
	return TrainingInfoPage(id).info
}

fun LuoGu.officialTraining(page: Int = 1): TrainingListPage {
	return TrainingListPage(page, TrainingListPage.Type.Official, client)
}

fun LuoGu.publicTraining(page: Int = 1): TrainingListPage {
	return TrainingListPage(page, TrainingListPage.Type.Public, client)
}

fun LuoGu.training(id: Int): TrainingInfoPage {
	return TrainingInfoPage(id, client)
}

internal fun url(id: Int, mark: Boolean) = "api/training/${if (mark) "mark" else "unmark"}/$id"

suspend fun LuoGu.markTraining(id: Int) {
	apiPost(url(id, true)).receive<String>()
}

suspend fun LuoGu.unmarkTraining(id: Int) {
	apiPost(url(id, false)).receive<String>()
}