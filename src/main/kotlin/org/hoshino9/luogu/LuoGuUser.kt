package org.hoshino9.luogu

import org.jsoup.nodes.Element

open class LuoGuUser(val uid : String) {
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
	val passedProblems : List<String> get() = TODO()

	/**
	 * 尝试过的题目
	 * 返回一个 List
	 * 题目的pid
	 */
	val triedProblems : List<String> get() = TODO()

	override fun toString() : String {
		return uid
	}
}