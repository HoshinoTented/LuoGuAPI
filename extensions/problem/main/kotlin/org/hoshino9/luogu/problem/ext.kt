@file:JvmName("ProblemUtils")

package org.hoshino9.luogu.problem

import org.hoshino9.luogu.IllegalStatusCodeException
import org.hoshino9.luogu.LuoGu

/**
 * 题目列表
 * @param page 页数, 默认为 **1**
 * @param filter 过滤器
 * @throws IllegalStatusCodeException
 * @return 返回题目列表
 *
 * @see IProblem
 * @see ProblemSearchConfig
 */
@JvmOverloads
fun LuoGu.problemList(page: Int = 1, filter: ProblemSearchConfig = ProblemSearchConfig()): ProblemList {
	return ProblemList(page, filter, client)
}