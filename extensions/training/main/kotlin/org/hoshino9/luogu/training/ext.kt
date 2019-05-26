@file:JvmName("TrainingUtils")

package org.hoshino9.luogu.training

import org.hoshino9.luogu.LuoGu

val LuoGu.trainingPage: TrainingPage
	get() {
		return DefaultTrainingPage(this)
	}