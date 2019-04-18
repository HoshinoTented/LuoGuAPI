package org.hoshino9.luogu.experimental.problem

import okhttp3.OkHttpClient
import org.hoshino9.luogu.page.ExperimentalLuoGuPage
import org.hoshino9.luogu.problem.ProblemSearchConfig
import org.hoshino9.luogu.utils.emptyClient

open class ProblemListPage(val page: Int, val filter: ProblemSearchConfig, override val client: OkHttpClient = emptyClient) : ExperimentalLuoGuPage() {
	override val url: String get() = "https://www.luogu.org/fe/problem/list?page=p$page&$filter"


}