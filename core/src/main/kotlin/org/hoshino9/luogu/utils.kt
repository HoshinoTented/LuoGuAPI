@file:Suppress("MemberVisibilityCanBePrivate", "unused")

package org.hoshino9.luogu

import org.jsoup.nodes.Document

const val domain = ".luogu.com.cn"
const val baseUrl = "https://www$domain"
const val wsUrl = "wss://ws$domain/ws"


/**
 * 一个奇怪的Token, 似乎十分重要, 大部分操作都需要这个
 * @param page 任意一个**你谷**页面
 * @return 返回 `csrf-token`
 */
fun csrfTokenFromPage(page: Document): String {
	return page.head().getElementsByTag("meta").firstOrNull { it?.attr("name") == "csrf-token" }?.attr("content")
			?: throw HTMLParseException(page)
}