package org.hoshino9.luogu.problem

import org.hoshino9.luogu.tag.LuoGuTag

/**
 * **你谷** 题目列表搜索配置
 */
data class ProblemSearchConfig @JvmOverloads constructor(
		val keyword: String = "",
		val type: Type = Type.LuoGu,
		val difficulty: Difficulty? = null,        // null 表示全部
		val ordering: Ordering? = null,
		val tags: Tags? = null
) {
	data class Tags(val algo: List<LuoGuTag>, val from: List<LuoGuTag>, val time: List<LuoGuTag>, val area: List<LuoGuTag>) {
		override fun toString(): String {
			return listOf(algo, from, time, area).filter { it.isNotEmpty() }.joinToString(separator = "|") { group ->
				group.joinToString(separator = ",") { it.id.toString() }
			}
		}
	}

	data class Ordering(val sortBy: SortMode, val ordBy: OrdMode) {
		enum class SortMode {
			PID,
			NAME,
			DIFFICULTY
		}

		enum class OrdMode {
			ASC,        // 升序
			DESC        // 降序
		}

		override fun toString(): String {
			return "orderBy=${sortBy.name.toLowerCase()}&order=${ordBy.name.toLowerCase()}"
		}
	}

	override fun toString() : String {
		return buildString {
			append("keyword", "=", keyword, "&")
			append("type", "=", type.id, "&")
			if (difficulty != null) append("difficulty", "=", Difficulty.values().indexOf(difficulty), "&")
			if (ordering != null) append(ordering, "&")
			if (tags != null) append(tags, "&")
		}
	}
}