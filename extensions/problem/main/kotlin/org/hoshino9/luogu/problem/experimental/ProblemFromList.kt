package org.hoshino9.luogu.problem.experimental

import org.hoshino9.luogu.tag.IdLuoGuTag
import org.hoshino9.luogu.tag.LuoGuTag
import org.hoshino9.luogu.utils.delegate
import org.json.JSONArray
import org.json.JSONObject

open class ProblemFromList(
		override val pid: String,
		override val difficulty: Difficulty,
		override val tags: List<LuoGuTag>,
		override val name: String,
		override val totalAccepted: Long,
		override val totalSubmit: Long,
		override val type: Int,
		override val wantsTranslation: Boolean
) : Problem {
	companion object {
		operator fun invoke(data: JSONObject): ProblemFromList {
			return data.delegate.let { obj ->
				val pid: String by obj
				val difficulty: Int by obj
				val tags: JSONArray by obj
				val title: String by obj
				val totalAccepted: String by obj
				val totalSubmit: String by obj
				val type: Int by obj
				val wantsTranslation: Boolean by obj

				ProblemFromList(
						pid,
						Difficulty.values()[difficulty],
						tags.map { IdLuoGuTag(it as Int) },
						title,
						totalAccepted.toLong(),
						totalSubmit.toLong(),
						type,
						wantsTranslation
				)
			}
		}
	}
}