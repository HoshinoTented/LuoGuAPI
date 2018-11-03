@file:Suppress("unused")

package org.hoshino9.luogu.problems

import org.hoshino9.luogu.LuoGuTag
import org.hoshino9.luogu.interfaces.HasElement
import org.jsoup.nodes.Element

/**
 * `AbstractProblem` 的子类
 *
 * @param elem 题目的 html 元素
 */
open class ParsedProblem(override val elem : Element) : AbstractProblem(), HasElement {
	companion object {
		private val passPercentRegex = Regex("""(\d+) /(\d+)""")
	}

	private val elemMain : Element by lazy { elem.child(0) }
	private val passBlock : Element by lazy { elem.child(1) }

	override val pid : String by lazy {
		elem
				.children()
				.first()
				?.textNodes()
				?.firstOrNull { it.text().isNotBlank() }
				?.text()
				?.trim() ?: throw NoSuchElementException()
	}

	override val difficulty : Problem.Difficulty by lazy { TODO() }
	override val name : String by lazy { TODO() }
	override val passPercent : Pair<Long, Long> by lazy {
		passBlock.child(0).child(0).text().run(passPercentRegex::matchEntire)?.let {
			it.groupValues[1].toLong() to it.groupValues[2].toLong()
		} ?: - 1L to - 1L
	}
	override val tags : List<LuoGuTag> by lazy { TODO() }
}