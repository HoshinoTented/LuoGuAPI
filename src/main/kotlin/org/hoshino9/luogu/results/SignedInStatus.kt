package org.hoshino9.luogu.results

import org.jsoup.nodes.Element
import org.jsoup.select.Elements

data class SignedInStatus(val qian : Qian, val goods : List<Thing>, val bads : List<Thing>, val continuation : Int) {
	companion object Parser {
		private val regex = Regex("[宜忌]：([^ ]+) ([^ ]+)")

		/**
		 * 解析html代码并实例化一个 `SignedInStatus`
		 * 接受一个 `<div class="am-u-md-4 lg-punch am-text-center">...</div>` 的子元素集合
		 * @param page
		 * @return SignedInStatus
		 *
		 * @see Elements
		 * @see SignedInStatus
		 */
		@Throws(IllegalStateException::class)
		@JvmName("newInstance")
		operator fun invoke(page : Elements) : SignedInStatus {
			val head = page.getOrNull(1) ?: throw NoSuchElementException("second element of $page")
			val body = page.lastOrNull() ?: throw NoSuchElementException("last element of $page")

			if (head.tagName() == "span") {
				val headText = head.text().substring(2..3)
				val qian = Qian.values().firstOrNull { it.show == headText } ?: throw NoSuchElementException(headText)

				val goods = body.children().getOrNull(0) ?: throw NoSuchElementException("first element of $body")
				val bads = body.children().getOrNull(1) ?: throw NoSuchElementException("second element of $body")
				val bottom = body.children().getOrNull(2) ?: throw NoSuchElementException("third element of $body")

				return SignedInStatus(
						qian,
						goods.run(::parseThings),
						bads.run(::parseThings),
						bottom.children().firstOrNull { it.tagName() == "strong" }?.text()?.toInt() ?: throw NoSuchElementException("first element of tag name 'strong' in $bottom")
				)
			} else throw IllegalStateException("no signed in")
		}

		private fun parseThings(node : Element) : List<Thing> {
			return regex.findAll(node.text()).map {
				Thing(it.groupValues[1], it.groupValues[2])
			}.toList()
		}
	}

	enum class Qian(val show : String) {
		VeryGood("大吉"),
		MidGood("中吉"),
		Good("小吉"),
		Middle("中平"),
		Bad("凶"),
		VeryBad("大凶"),
	}

	data class Thing(val name : String, val description : String)
}