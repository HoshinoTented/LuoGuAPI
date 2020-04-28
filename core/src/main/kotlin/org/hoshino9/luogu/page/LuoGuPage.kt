package org.hoshino9.luogu.page

import com.google.gson.JsonObject

/**
 * 洛谷页面接口
 *
 * [refresh] 应该重新获取网页内容，并更新 [feInjection]
 *
 * 事实上一般用不到这个 [refresh]，主要是 [org.hoshino9.luogu.LuoGu] 的刷新问题，
 * 以后可能通过改善设计来绕过。
 */
interface LuoGuPage {
	companion object;

	val url: String
	val feInjection: JsonObject

	fun refresh()
}

val LuoGuPage.currentData: JsonObject get() = feInjection["currentData"].asJsonObject