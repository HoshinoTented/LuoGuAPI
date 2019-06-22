@file:Suppress("MemberVisibilityCanBePrivate", "unused")

package org.hoshino9.luogu

import okhttp3.HttpUrl
import org.hoshino9.luogu.data.SliderPhoto
import org.hoshino9.luogu.user.User
import org.jsoup.nodes.Document

object LuoGuUtils {
	const val domain = "www.luogu.org"
	const val baseUrl = "https://$domain"
	val httpUrl: HttpUrl get() = HttpUrl.get(baseUrl)

	fun lastValueFromUrl(url: String): String {
		return url.substringAfterLast('=')
	}

	fun nameFromUrl(url: String): String {
		return url.substringAfterLast('/')
	}

	fun userFromUrl(url: String): User {
		return User(lastValueFromUrl(url))
	}

	/**
	 * 解析主站滚动图片
	 * @param page 主站页面
	 * @return 返回一个列表, 其中 Pair 的 first 是链接, second 是图片
	 *
	 * @see Document
	 */
	fun sliderPhotosFromPage(page: Document): List<SliderPhoto> {
		val name = "lg-slider"
		val outerElement = page.getElementById(name) ?: throw NoSuchElementException(name)
		val element = outerElement.children().firstOrNull() ?: throw NoSuchElementException("first child of $name")
		return element.children().filter {
			it.className() != "clone"
		}.mapNotNull {
			val linkElement = it.children().firstOrNull() ?: return@mapNotNull null
			val imgElement = linkElement.children().firstOrNull()
			if (imgElement == null) {
				SliderPhoto(null, linkElement.attr("src"))
			} else {
				SliderPhoto(linkElement.attr("href"), imgElement.attr("src"))
			}
		}
	}

	/**
	 * 获取 user
	 * @param document Document对象, 即**你谷**主站页面, 因为**你谷**某些奇怪的原因, 而没有开放API, 所以只能从网页中爬了
	 * @return 返回一个uid, **Nullable**
	 *
	 * @see Document
	 */
	fun userIdFromPage(document: Document): String? {
		return (document.body()
				.getElementsByAttribute("myuid")
				.firstOrNull()
				?.attr("myuid")
				.orEmpty()).takeIf { it.isNotEmpty() }
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