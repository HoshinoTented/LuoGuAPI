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
		private val passPercentRegex = Regex("""(\d+) /(\d+)""")

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

	private val elemMain : Element by lazy { elem.child(0) }
	private val passBlock : Element by lazy { elem.child(1) }

	override val difficulty : Difficulty by lazy { TODO() }
	override val name : String by lazy { TODO() }
	override val passPercent : Pair<Long, Long> by lazy {
		passBlock.child(0).child(0).text().run(passPercentRegex::matchEntire)?.let {
			it.groupValues[1].toLong() to it.groupValues[2].toLong()
		} ?: - 1L to - 1L
	}
	override val tags : List<LuoGuTag> by lazy { TODO() }
}