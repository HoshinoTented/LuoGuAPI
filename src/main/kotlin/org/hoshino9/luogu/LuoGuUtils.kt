package org.hoshino9.luogu

import org.hoshino9.luogu.record.Record
import org.jsoup.nodes.Document

object LuoGuUtils {
	fun getUserFromUrl(url : String) : User {
		return url.substring(url.lastIndexOf('=') + 1).run(::User)
	}

	/**
	 * 解析主站滚动图片
	 * @param page 主站页面
	 * @return 返回一个列表, 其中 Pair 的 first 是链接, second 是图片
	 *
	 * @see Document
	 */
	fun getSliderPhotosFromPage(page : Document) : List<SliderPhoto> {
		val name = "lg-slider"
		return page.getElementById(name)?.run {
			children().first()?.run {
				children().filter {
					it.className() != "clone"
				}.mapNotNull {
					val linkElement = it.children().first() ?: return@mapNotNull null
					val imgElement = linkElement.children().first() ?: null
					if (imgElement == null) {
						SliderPhoto(null, linkElement.attr("src"))
					} else {
						SliderPhoto(linkElement.attr("href"), imgElement.attr("src"))
					}
				}
			} ?: throw NoSuchElementException("first child of $name")
		} ?: throw NoSuchElementException(name)
	}

	/**
	 * 获取 user
	 * @param document Document对象, 即**你谷**主站页面, 因为**你谷**某些奇怪的原因, 而没有开放API, 所以只能从网页中爬了
	 * @return 返回一个uid, **Nullable**
	 *
	 * @see Document
	 */
	fun getUserIdFromPage(document : Document) : String? {
		return (document.body()
				.getElementsByAttribute("myuid")
				.first()
				?.attr("myuid") ?: "").takeIf { it != "" }
	}

	/**
	 * **TODO** move to org.hoshino9.luogu.record
	 * 因为**你谷**不让别人爬评测记录, 所以就不能爬了
	 * 获取评测记录
	 * @param page Document对象, 即**你谷**主站页面
	 * @param filter 用于过滤的函数
	 * @return 返回一个评测记录列表
	 *
	 * @see Document
	 * @see Record
	 */
	@Suppress("unused", "UNUSED_PARAMETER")
	inline fun getRecordsFromPage(page : Document, filter : (Record) -> Boolean = { true }) : List<Record> {
		TODO("""评测记录相关页面不欢迎一切爬虫行为。
我相信如果你正在制作爬虫，一定能够看到本段文字。
请勿再制作任何爬取评测记录的爬虫。""")
	}

	/**
	 * 一个奇怪的Token, 似乎十分重要, 大部分操作都需要这个
	 * @param page 任意一个**你谷**页面
	 * @return 返回 `csrf-token`, 若找不到则返回 **null**
	 */
	fun getCsrfTokenFromPage(page : Document) : String {
		return page.head().getElementsByTag("meta").firstOrNull { it?.attr("name") == "csrf-token" }?.attr("content") ?: throw HTMLParseException(page)
	}
}