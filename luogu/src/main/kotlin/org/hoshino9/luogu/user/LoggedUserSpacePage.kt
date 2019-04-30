@file:Suppress("MemberVisibilityCanBePrivate")

package org.hoshino9.luogu.user

import org.hoshino9.luogu.LuoGuUtils
import org.hoshino9.luogu.problem.Problem
import org.hoshino9.luogu.utils.page
import org.jsoup.nodes.Document

class LoggedUserSpacePage(val loggedUser : LoggedUser) : UserSpacePage(loggedUser) {
	override val page : Document = run {
		loggedUser.luogu.client.page("${LuoGuUtils.baseUrl}/space/show?uid=${user.uid}")
	}

	override val username : String = run {
		feInjection.getJSONObject("currentUser").getString("name")
	}

	val introductionSrc : String = run {
		feInjection.getJSONObject("currentUser").getString("introduce")
	}

	/**
	 * 尝试过的题目
	 * 返回一个 List
	 * 题目的pid
	 */
	val triedProblems : List<Problem> = run {
		parseProblems(rights[2].children())
	}

	/**
	 * 咕值
	 */
	val gugugu : Int = run {
		elem.getElementById("highchart-guzhi").parent().children().last().child(0).text().toInt()
	}
}