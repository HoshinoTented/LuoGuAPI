@file:JvmName("ProblemUtils")

package org.hoshino9.luogu.problem

import com.google.gson.JsonObject
import io.ktor.client.call.receive
import org.hoshino9.luogu.IllegalStatusCodeException
import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.utils.apiPost
import org.hoshino9.luogu.utils.asParams
import org.hoshino9.luogu.utils.referer

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
fun LuoGu.problemList(page: Int = 1, filter: ProblemSearchConfig = ProblemSearchConfig()): ProblemList {
	return ProblemList(page, filter, client)
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