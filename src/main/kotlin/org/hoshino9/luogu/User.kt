@file:Suppress("unused")

package org.hoshino9.luogu

import org.hoshino9.luogu.problems.Problem
import org.hoshino9.okhttp.LuoGuOnlyCookieJar
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.jsoup.nodes.Element

open class User(val uid : String) {
	private val page : Document by lazy {
		defaultClient.getExecute("https://${LuoGuOnlyCookieJar.domain}/space/show?uid=$uid") { resp ->
			resp.assert()
			Jsoup.parse(resp.data !!)
		}
	}

	/**
	 * 用户名
	 */
	val username : String get() = TODO()

	/**
	 * 用户的签名
	 * **Nullable**
	 */
	val introduction : Element get() = TODO()

	/**
	 * 通过的题目
	 * 返回一个 List
	 * 题目的pid
	 */
	val passedProblems : List<Problem> get() = TODO()

	/**
	 * 尝试过的题目
	 * 返回一个 List
	 * 题目的pid
	 */
	val triedProblems : List<Problem> get() = TODO()

	/**
	 * 咕值
	 */
	val gugugu : Int get() = TODO()

	override fun equals(other : Any?) : Boolean {
		if (this === other) return true
		if (other !is User) return false

		if (uid != other.uid) return false

		return true
	}

	override fun hashCode() : Int {
		return uid.hashCode()
	}

	override fun toString() : String {
		return uid
	}
}
