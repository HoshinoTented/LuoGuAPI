package org.hoshino9.luogu.problems

import org.jsoup.nodes.Document
import org.jsoup.nodes.Element

open class ProblemListPage(val page : Document) {
	private val listClass = "lg-content-table-left"

	fun list() : List<Problem> {
		return page.body().getElementsByClass(listClass).first()?.children()?.dropLast(1)?.map { elem ->
			ParsedProblem(elem)
		} ?: throw NoSuchElementException(listClass)
	}
}