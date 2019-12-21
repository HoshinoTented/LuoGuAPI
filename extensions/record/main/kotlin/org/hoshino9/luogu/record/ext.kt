@file:JvmName("RecordKt")

package org.hoshino9.luogu.record

import com.google.gson.JsonObject
import io.ktor.client.call.receive
import org.hoshino9.luogu.IllegalStatusCodeException
import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.user.LoggedUser
import org.hoshino9.luogu.utils.*

/**
 * 提交题解
 * @param solution 题解对象
 * @return 返回 Record 对象
 *
 * @see Solution
 * @see Record
 */
@JvmOverloads
suspend fun LuoGu.postSolution(solution: Solution, verifyCode: String = ""): Record {
	val params = listOf(
			"code" to solution.code,
			"lang" to Solution.Language.values().indexOf(solution.language).toString(),
			"enableO2" to if (solution.enableO2) "1" else "0",
			"verify" to verifyCode
	).asParams

	return apiPost("api/problem/submit/${solution.pid}") {
		referer("problem/${solution.pid}")
		body = params
	}.receive<String>().let {
		json(it).run {
			val status = this["status"]?.asInt //optInt("status")
			val data = this["data"]

			if (status == 200) {
				Record(data.asJsonObject["rid"].toString())
			} else throw IllegalStatusCodeException(status, data)
		}
	}
}