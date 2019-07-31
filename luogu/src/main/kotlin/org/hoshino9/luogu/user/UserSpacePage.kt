@file:Suppress("unused", "MemberVisibilityCanBePrivate")

package org.hoshino9.luogu.user

import org.hoshino9.luogu.LuoGuUtils.baseUrl
import org.hoshino9.luogu.page.AbstractLuoGuPage
import org.hoshino9.luogu.utils.emptyClient
import org.jsoup.nodes.Element
import org.jsoup.select.Elements

// FIXME: UserSpacePage 合并到 User 里

open class UserSpacePage(val user: User) : AbstractLuoGuPage() {
	override val url: String = "$baseUrl/space/show?uid=${user.uid}"

	val elem: Element by lazy {
		page.body()
	}

	val rights: Elements by lazy {
		elem.getElementsByClass("lg-article am-hide-sm")
	}

	/**
	 * 用户名
	 */
	open val username : String by lazy {
		feInjection.get("currentMeta").asJsonObject.get("title").asString.dropLast(4)
	}

	/**
	 * 用户的签名
	 * **Nullable**
	 */
	open val introduction : Element by lazy {
		TODO()
	}

	val avatar: String get() = "https://cdn.luogu.org/upload/usericon/${user.uid}.png"
}