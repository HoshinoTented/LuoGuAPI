@file:JvmName("TrainingUtils")

package org.hoshino9.luogu.training

import org.hoshino9.luogu.utils.HttpClient
import org.hoshino9.luogu.utils.emptyClient

fun BaseTraining.lift(client: HttpClient = emptyClient): TrainingInfo {
	return TrainingInfoPage(id).info
}