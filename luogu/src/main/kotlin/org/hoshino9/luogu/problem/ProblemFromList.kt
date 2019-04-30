@file:Suppress("unused")

package org.hoshino9.luogu.problem

import org.hoshino9.luogu.problem.tags.parseTags
import org.hoshino9.luogu.tag.ColoredLuoGuTag
import org.hoshino9.luogu.utils.HasElement
import org.hoshino9.luogu.utils.HttpClient
import org.hoshino9.luogu.utils.percentRegex
import org.jsoup.nodes.Document
import org.jsoup.nodes.Element

/**
 * `AbstractProblem` 的子类
 *
 * @param elem 题目的 html 元素(题目列表)
 */
open class ProblemFromList(override val elem: Element, client: HttpClient) : AbstractProblem(client), HasElement {
	private val elemMain : Element = run { elem.child(0) }
	private val passBlock : Element = run { elem.child(1) }

	override val page : Document = run {
		ProblemFromId(id, client).page
	}

	override val id : String = run {
		elem
				.children()
				.first()
				?.textNodes()
				?.firstOrNull { it.text().isNotBlank() }
				?.text()
				?.trim() ?: throw NoSuchElementException()
	}

	// 获取最后一个 tag
	override val difficulty : Problem.Difficulty = run { tags.first { it is Problem.Difficulty } as Problem.Difficulty }
	override val name : String = run { elemMain.children().let { it[it.size - 2] }.text() }
	override val passPercent : Pair<String, String> = run {
		passBlock.child(0).child(0).text().run(percentRegex::matchEntire)?.let {
			it.groupValues[1] to it.groupValues[2]
		} ?: "" to ""
	}


	override val tags: List<ColoredLuoGuTag> = run {
		elemMain.children().last().run(::parseTags)
	}
}