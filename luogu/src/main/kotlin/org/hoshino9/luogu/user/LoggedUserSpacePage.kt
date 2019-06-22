@file:Suppress("MemberVisibilityCanBePrivate")

package org.hoshino9.luogu.user

import org.hoshino9.luogu.LuoGuUtils
import org.hoshino9.luogu.problem.Problem
import org.hoshino9.luogu.utils.page
import org.jsoup.nodes.Document

// FIXME: LoggedUserSpacePage 合并到 LoggedUser 里

class LoggedUserSpacePage(val loggedUser : LoggedUser) : UserSpacePage(loggedUser) {
	override val page : Document by lazy {
		loggedUser.luogu.client.page("${LuoGuUtils.baseUrl}/space/show?uid=${user.uid}")
	}

	override val username : String by lazy {
		feInjection.getJSONObject("currentUser").getString("name")
	}

	val introductionSrc : String by lazy {
		feInjection.getJSONObject("currentUser").getString("introduce")
	}

	/**
	 * 尝试过的题目
	 * 返回一个 List
	 * 题目的pid
	 */
	val triedProblems : List<Problem> by lazy {
		parseProblems(rights[2].children())
	}

	/**
	 * 咕值
	 */
	val gugugu : Int by lazy {
		elem.getElementById("highchart-guzhi").parent().children().last().child(0).text().toInt()
	}
}