@file:Suppress("unused")

package org.hoshino9.luogu.problem

import org.hoshino9.luogu.LuoGuUtils.baseUrl
import org.hoshino9.luogu.page.AbstractLuoGuPage
import org.hoshino9.luogu.utils.HttpClient

abstract class AbstractProblem(client: HttpClient) : AbstractLuoGuPage(client), Problem {
	/**
	 * 题目的地址
	 */
	override val url: String get() = "$baseUrl/problemnew/show/$id"

	override val content : ProblemContent by lazy {
		ProblemContent.parse(id)
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