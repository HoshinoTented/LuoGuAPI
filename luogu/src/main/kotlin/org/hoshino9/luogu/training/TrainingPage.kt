package org.hoshino9.luogu.training

import org.hoshino9.luogu.LuoGu

interface TrainingPage {
	companion object {
		@JvmName("newInstance")
		operator fun invoke(luogu : LuoGu) : TrainingPage {
			return DefaultTrainingPage(luogu)
		}
	}

	val trainingBlocks : List<TrainingBlock>
	val passedCount : String
	val lastPassedTime : String
	val skipPercent : Pair<String, String>
}