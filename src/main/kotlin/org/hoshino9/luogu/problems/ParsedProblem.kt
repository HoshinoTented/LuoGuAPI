package org.hoshino9.luogu.problems

import org.hoshino9.luogu.problems.tags.LuoGuTag
import org.jsoup.nodes.Element

/**
 * `Problem` 的子类
 *
 * @param elem 题目的 html 元素
 */
open class ParsedProblem(val elem : Element) : Problem(pid(elem)) {
	companion object {
		fun pid(elem : Element) : String {
			return elem
					.children()
					.first()
					?.textNodes()
					?.firstOrNull { it.text().isNotBlank() }
					?.text()
					?.trim() ?: throw NoSuchElementException()
		}
	}

	override val difficulty : Difficulty by lazy { TODO() }
	override val name : String by lazy { TODO() }
	override val passPercent : Pair<Long, Long> by lazy { TODO() }
	override val tags : List<LuoGuTag> by lazy { TODO() }
}