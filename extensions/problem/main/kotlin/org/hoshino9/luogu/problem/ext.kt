@file:JvmName("ProblemUtils")

package org.hoshino9.luogu.problem

import com.google.gson.JsonObject
import io.ktor.client.call.receive
import org.hoshino9.luogu.IllegalStatusCodeException
import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.utils.*

fun BaseProblem.lift(client: HttpClient = emptyClient): Problem = run {
	if (this is Problem) this
	else ProblemPageBuilder(pid, client).build().problem
}

fun BaseProblem.liftToLogged(client: HttpClient = emptyClient): LoggedProblem = run {
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
@JvmOverloads
fun LuoGu.problemList(page: Int = 1, filter: ProblemSearchConfig = ProblemSearchConfig()): ProblemListPage {
	return ProblemListPageBuilder(page, filter, client).build()
}

internal suspend fun LuoGu.doMark(pid: String, mark: Boolean) {
	apiPost("fe/api/problem/${if (mark) "tasklistAdd" else "tasklistRemove"}") {
		referer("problem/$pid")

		body = JsonObject().apply {
			addProperty("pid", pid)
		}.asParams
	}.receive<String>().run(::println)
}

/**
 * 收藏题目
 */
suspend fun LuoGu.mark(pid: String) = doMark(pid, true)

/**
 * 取消收藏题目
 */
suspend fun LuoGu.unmark(pid: String) = doMark(pid, false)