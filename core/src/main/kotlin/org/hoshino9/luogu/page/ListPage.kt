package org.hoshino9.luogu.page

import kotlin.math.ceil

interface ListPage {
	companion object;

	/**
	 * 搜索结果数量
	 */
	val count: Int

	/**
	 * 每个显示元素数量
	 */
	val perPage: Int
}

val ListPage.maxPageCount
	get() = run {
		ceil(count.toDouble() / perPage).toInt()
	}