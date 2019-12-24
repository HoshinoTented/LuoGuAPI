@file:JvmName("RecordUtils")

package org.hoshino9.luogu.record

import com.google.gson.JsonObject
import io.ktor.client.call.receive
import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.utils.*

/**
 * 提交题解
 * @param solution 题解对象
 * @return 返回 Record 对象
 *
 * @see Solution
 * @see Record
 */
suspend fun LuoGu.postSolution(solution: Solution, verifyCode: String = ""): Record {
	val json = JsonObject().apply {
		addProperty("verify", verifyCode)
		addProperty("enableO2", if (solution.enableO2) 1 else 0)
		addProperty("lang", solution.language)
		addProperty("code", solution.code)
	}.asParams

	return apiPost("fe/api/problem/submit/${solution.pid}") {
		referer("problem/${solution.pid}")
		body = json
	}.receive<String>().let {
		json(it).run {
			val rid: String by provider

			Record(rid)
		}
	}
}