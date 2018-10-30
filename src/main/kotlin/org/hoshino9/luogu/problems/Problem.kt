@file:Suppress("unused")

package org.hoshino9.luogu.problems

import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.problems.tags.LuoGuTag

abstract class Problem(open val pid : String) {
	abstract class Difficulty(text : String) : LuoGuTag(text, - 1)
	object Red : Difficulty("入门难度")
	object Orange : Difficulty("普及-")
	object Yellow : Difficulty("普及/提高-")
	object Green : Difficulty("普及+/提高")
	object Blue : Difficulty("提高+/省选-")
	object Purple : Difficulty("省选/NOI-")
	object Black : Difficulty("NOI/NOI+/CTSC")

	/**
	 * 难度
	 */
	abstract val difficulty : Difficulty

	/**
	 * 题目名称
	 */
	abstract val name : String

	/**
	 * 题目通过率
	 */
	abstract val passPercent : Pair<Long, Long>

	/**
	 * 题目的标签
	 */
	abstract val tags : List<LuoGuTag>

	/**
	 * 题目的地址
	 */
	open val url : String get() = "${LuoGu.baseUrl}/problemnew/show/$pid"

	override fun equals(other : Any?) : Boolean {
		return (other as? Problem)?.pid == pid
	}

	override fun hashCode() : Int {
		return pid.hashCode()
	}

	override fun toString() : String {
		return pid
	}
}