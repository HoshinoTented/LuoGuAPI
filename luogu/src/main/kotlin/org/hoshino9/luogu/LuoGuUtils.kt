@file:Suppress("MemberVisibilityCanBePrivate", "unused")

package org.hoshino9.luogu

import okhttp3.HttpUrl
import okhttp3.HttpUrl.Companion.toHttpUrl
import org.hoshino9.luogu.user.User
import org.jsoup.nodes.Document

object LuoGuUtils {
	const val domain = "www.luogu.org"
	const val baseUrl = "https://$domain"
	val httpUrl: HttpUrl get() = baseUrl.toHttpUrl()

	fun lastValueFromUrl(url: String): String {
		return url.substringAfterLast('=')
	}

	/**
	 * 一个奇怪的Token, 似乎十分重要, 大部分操作都需要这个
	 * @param page 任意一个**你谷**页面
	 * @return 返回 `csrf-token`, 若找不到则返回 **null**
	 */
	fun csrfTokenFromPage(page: Document): String {
		return page.head().getElementsByTag("meta").firstOrNull { it?.attr("name") == "csrf-token" }?.attr("content")
				?: throw HTMLParseException(page)
	}
}