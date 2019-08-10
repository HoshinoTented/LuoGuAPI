@file:JvmName("RecordKt")

package org.hoshino9.luogu.record

import com.google.gson.JsonObject
import org.hoshino9.luogu.IllegalStatusCodeException
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
fun LoggedUser.postSolution(solution: Solution, verifyCode: String = ""): Record {
	return luogu.executePost("api/problem/submit/${solution.pid}",
			listOf(
					"code" to solution.code,
					"lang" to Solution.Language.values().indexOf(solution.language).toString(),
					"enableO2" to if (solution.enableO2) "1" else "0",
					"verify" to verifyCode
			).params(), referer("problem/${solution.pid}")
	) { resp ->
		resp.assert()
		val content = resp.strData

		json(content) {
			val status = this["status"]?.asInt //optInt("status")
			val data = this["data"]

			if (status == 200) {
				data as JsonObject
				Record(data["rid"].toString())
			} else throw IllegalStatusCodeException(status, data)
		}
	}
}