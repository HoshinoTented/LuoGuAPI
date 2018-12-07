@file:Suppress("unused")

package org.hoshino9.luogu.problems

import org.hoshino9.luogu.LuoGuUtils.baseUrl

abstract class AbstractProblem : Problem {
	/**
	 * 题目的地址
	 */
	@Suppress("SpellCheckingInspection")
	open val url : String get() = "$baseUrl/problemnew/show/$id"

	override val content : ProblemContent by lazy {
		DefaultProblemContent(id)
	}

	override fun equals(other : Any?) : Boolean {
		return (other as? AbstractProblem)?.id == id
	}

	override fun hashCode() : Int {
		return id.hashCode()
	}

	override fun toString() : String {
		return id
	}
}