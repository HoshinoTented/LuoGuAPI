package org.hoshino9.luogu.problems

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


}