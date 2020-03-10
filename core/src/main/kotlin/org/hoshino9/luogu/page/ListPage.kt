package org.hoshino9.luogu.page

interface ListPage {
	/**
	 * 搜索结果数量
	 */
	val count: Int

	/**
	 * 每个显示元素数量
	 */
	val perPage: Int
}