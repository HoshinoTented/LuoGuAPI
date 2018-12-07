@file:Suppress("unused")

package org.hoshino9.luogu.training

import org.hoshino9.luogu.*
import org.hoshino9.luogu.problems.Problem

interface Training {
	enum class Status(val content : String) {
		ALL_KILL("已通过"),
		CHALLENGING("可挑战"),
		DISABLE("有先决要求"),
		SKIP("暂时跳过"),
	}

	companion object {
		@JvmName("newInstance")
		operator fun invoke(mid : String, luogu : LuoGu) : Training {
			return DefaultTraining(mid, luogu)
		}
	}

	val mid : String
	val name : String
	val status : Status
	val problems : List<Problem>
}