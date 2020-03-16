package org.hoshino9.luogu.page

import kotlin.math.ceil

/**
 * 列表页面，仅用于数据类接口
 */
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

/**
 * 根据搜索结果数量和每页显示数量推断总页数
 */
val ListPage.maxPageCount
	get() = run {
		ceil(count.toDouble() / perPage).toInt()
	}