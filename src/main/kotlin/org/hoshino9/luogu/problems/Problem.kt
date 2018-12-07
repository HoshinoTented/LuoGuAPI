package org.hoshino9.luogu.problems

import org.hoshino9.luogu.LuoGuTag

interface Problem {
	abstract class Difficulty(text : String) : LuoGuTag(text, - 1)
	object Red : Difficulty("入门难度")
	object Orange : Difficulty("普及-")
	object Yellow : Difficulty("普及/提高-")
	object Green : Difficulty("普及+/提高")
	object Blue : Difficulty("提高+/省选-")
	object Purple : Difficulty("省选/NOI-")
	object Black : Difficulty("NOI/NOI+/CTSC")

	val id : String

	/**
	 * 难度
	 */
	val difficulty : Difficulty

	/**
	 * 题目名称
	 */
	val name : String

	/**
	 * 题目通过率
	 */
	val passPercent : Pair<String, String>

	/**
	 * 题目的标签
	 */
	val tags : List<LuoGuTag>

	/**
	 * 题目内容
	 */
	val content : ProblemContent
}