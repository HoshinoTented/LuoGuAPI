package org.hoshino9.luogu.training

import org.hoshino9.luogu.LuoGu
import org.jsoup.nodes.Element

interface TrainingBlock {
	companion object {
		@JvmName("newInstance")
		operator fun invoke(elem : Element, luogu : LuoGu) : TrainingBlock {
			return DefaultTrainingBlock(elem, luogu)
		}
	}

	val trainingLv : String
	val name : String
	val description : String
	val passPercent : Pair<String, String>
	val trainings : List<Training>
}