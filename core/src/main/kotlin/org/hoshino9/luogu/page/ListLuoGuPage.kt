package org.hoshino9.luogu.page

import org.hoshino9.luogu.utils.HttpClient
import org.hoshino9.luogu.utils.emptyClient
import kotlin.math.ceil

abstract class ListLuoGuPage(client: HttpClient = emptyClient) : AbstractLuoGuPage(client), ListPage {
	val maxPageCount: Int
		get() = run {
			ceil(count.toDouble() / perPage).toInt()
		}
}