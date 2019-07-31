@file:Suppress("MemberVisibilityCanBePrivate")

package org.hoshino9.luogu.user

import org.hoshino9.luogu.LuoGuUtils
import org.hoshino9.luogu.utils.page
import org.jsoup.nodes.Document

// FIXME: LoggedUserSpacePage 合并到 LoggedUser 里

class LoggedUserSpacePage(val loggedUser : LoggedUser) : UserSpacePage(loggedUser) {
	override val page : Document by lazy {
		loggedUser.luogu.client.page("${LuoGuUtils.baseUrl}/space/show?uid=${user.uid}")
	}

	override val username : String by lazy {
		feInjection.get("currentUser").asJsonObject.get("name").asString
	}

	val introductionSrc : String by lazy {
		feInjection.get("currentUser").asJsonObject.get("introduce").asString
	}

	/**
	 * 咕值
	 */
	val gugugu : Int by lazy {
		elem.getElementById("highchart-guzhi").parent().children().last().child(0).text().toInt()
	}
}