@file:Suppress("unused")

package org.hoshino9.luogu.problem

import org.hoshino9.luogu.color.luoguColor
import org.hoshino9.luogu.tag.LuoGuTag
import org.hoshino9.luogu.utils.HasElement
import org.jsoup.nodes.Element

/**
 * `AbstractProblem` 的子类
 *
 * @param elem 题目的 html 元素(题目列表)
 */
open class ProblemFromList(override val elem : Element) : AbstractProblem(), HasElement {
	companion object {
		private val passPercentRegex = Regex("""(\d+) /(\d+)""")
	}

	private val elemMain : Element by lazy { elem.child(0) }
	private val passBlock : Element by lazy { elem.child(1) }

	override val id : String by lazy {
		elem
				.children()
				.first()
				?.textNodes()
				?.firstOrNull { it.text().isNotBlank() }
				?.text()
				?.trim() ?: throw NoSuchElementException()
	}

	// 获取最后一个 tag
	override val difficulty : Problem.Difficulty by lazy { tags.first { it is Problem.Difficulty } as Problem.Difficulty }
	override val name : String by lazy { elemMain.children().let { it[it.size - 2] }.text() }
	override val passPercent : Pair<String, String> by lazy {
		passBlock.child(0).child(0).text().run(passPercentRegex::matchEntire)?.let {
			it.groupValues[1] to it.groupValues[2]
		} ?: "" to ""
	}


	override val tags : List<LuoGuTag> by lazy {
		elemMain.children().last().getElementsByClass("am-badge").map { elem ->
			if ("lg-tag" in elem.classNames()) {
				LuoGuTag(elem.text(), elem.attr("data-tagid").toInt(), elem.luoguColor !!.toColor())
			} else Problem.Difficulty(elem)
		}
	}
}