@file:JvmName("ProblemUtils")

package org.hoshino9.luogu.problem.experimental

import org.hoshino9.luogu.IllegalStatusCodeException
import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.user.LoggedUserSpacePage
import org.hoshino9.luogu.user.UserSpacePage
import org.hoshino9.luogu.utils.emptyClient
import org.jsoup.select.Elements

/**
 * 题目列表
 * @param page 页数, 默认为 **1**
 * @param filter 过滤器
 * @throws IllegalStatusCodeException
 * @return 返回题目列表
 *
 * @see Problem
 * @see ProblemSearchConfig
 */
@JvmOverloads
fun LuoGu.problemList(page: Int = 1, filter: ProblemSearchConfig = ProblemSearchConfig()): ProblemListPage {
	return ProblemListPage(page, filter, client)
}

/**
 * 通过的题目
 * 返回一个 List
 * 题目的pid
 */
fun UserSpacePage.passedProblems(): List<Problem> {
	return parseProblems(rights[0].children())
}

private fun parseProblems(es: Elements): List<Problem> {
	return es.mapNotNull { elem ->
		elem.takeIf { it.tagName() == "a" }?.text()?.let { Problem.Factory(it, emptyClient) }
	}
}

/**
 * 尝试过的题目
 * 返回一个 List
 * 题目的pid
 */
fun LoggedUserSpacePage.triedProblems(): List<Problem> {
	return parseProblems(rights[2].children())
}