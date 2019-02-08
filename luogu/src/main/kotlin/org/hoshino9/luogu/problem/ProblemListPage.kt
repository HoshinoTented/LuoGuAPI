package org.hoshino9.luogu.problem

import okhttp3.OkHttpClient
import org.hoshino9.luogu.LuoGuUtils
import org.hoshino9.luogu.page.AbstractLuoGuPage
import org.hoshino9.luogu.utils.HttpClient
import org.hoshino9.luogu.utils.page
import org.jsoup.nodes.Document

open class ProblemListPage(val pageCount : Int, val filter : ProblemSearchConfig, val client : HttpClient) : AbstractLuoGuPage() {
	private val listClass = "lg-content-table-left"

	override val page : Document by lazy {
		client.page("${LuoGuUtils.baseUrl}/problemnew/lists?$filter&page=$pageCount")
	}

	fun list() : List<Problem> {
		return page.body().getElementsByClass(listClass).first()?.children()?.dropLast(1)?.map { elem ->
			ProblemFromList(elem, client)
		} ?: throw NoSuchElementException(listClass)
	}
}