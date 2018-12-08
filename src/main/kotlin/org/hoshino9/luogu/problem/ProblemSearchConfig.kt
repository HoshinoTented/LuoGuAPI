package org.hoshino9.luogu.problem

import org.hoshino9.luogu.EQUAL
import org.hoshino9.luogu.SEPARATOR
import org.hoshino9.luogu.tag.LuoGuTag
import org.hoshino9.luogu.problem.tags.ProblemStore

/**
 * **你谷** 题目列表搜索配置
 * @param keyword 搜索的关键字
 * @param sortBy 排序方式
 * @param tags 标签筛选
 * @param searchContent 是否搜索题目内容(与 `keyword` 搭配)
 * @param type 题库
 *
 * @see LuoGuTag
 * @see ProblemStore
 * @see SortMode
 */
data class ProblemSearchConfig @JvmOverloads constructor(
		val keyword : String = "",
		val sortBy : SortMode = SortMode.PID,
		val tags : List<LuoGuTag> = emptyList(),
		val searchContent : Boolean = false,
		val type : List<ProblemStore> = emptyList()        //应该只是 `题库` 而已
) {
	enum class SortMode {
		PID,
		NAME,
		DIFFICULTY
	}

	override fun toString() : String {
		return buildString {
			listOf(
					"name", EQUAL, keyword, SEPARATOR,
					"orderitem", EQUAL, sortBy.name.toLowerCase(), SEPARATOR,
					"tag", EQUAL, tags.joinToString(separator = ",") { it.id.toString() }, SEPARATOR,
					"content", EQUAL, if (searchContent) "1" else "0", SEPARATOR,
					"type", EQUAL, type.joinToString(separator = "|") { it.id.toString() }
			).forEach { append(it) }
		}
	}
}