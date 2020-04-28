@file:JvmName("ProblemUtils")

package org.hoshino9.luogu.problem

import com.google.gson.JsonObject
import io.ktor.client.call.receive
import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.LuoGuClient
import org.hoshino9.luogu.baseUrl
import org.hoshino9.luogu.utils.*

fun BaseProblem.lift(client: LuoGuClient): Problem = run {
	if (this is Problem) this
	else ProblemPageBuilder(pid, client).build().problem
}

fun BaseProblem.liftToLogged(client: LuoGuClient): LoggedProblem = run {
	if (this is LoggedProblem) this
	else LoggedProblemPageBuilder(pid, client).build().problem
}

/**
 * 题目列表
 * @param page 页数, 默认为 **1**
 * @param filter 过滤器
 * @return 返回题目列表
 *
 * @see Problem
 * @see ProblemSearchConfig
 */
fun LuoGuClient.problemList(page: Int = 1, filter: ProblemSearchConfig = ProblemSearchConfig()): ProblemListPage {
	return ProblemListPageBuilder(page, filter, this).build()
}

internal suspend fun LuoGuClient.doMark(pid: String, mark: String) {
	post("$baseUrl/fe/api/problem/$mark",
			JsonObject().apply { addProperty("pid", pid) })
}

/**
 * 收藏题目
 */
suspend fun LuoGuClient.mark(pid: String) = doMark(pid, "tasklistAdd")

/**
 * 取消收藏题目
 */
suspend fun LuoGuClient.unmark(pid: String) = doMark(pid, "tasklistRemove")