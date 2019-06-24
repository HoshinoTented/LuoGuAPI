package org.hoshino9.luogu.problem.experimental

import org.hoshino9.luogu.tag.LuoGuTag

/**
 * 0:
difficulty: 1
pid: "P1421"
tags: Array(0)
title: "小玉买文具"
totalAccepted: "95590"
totalSubmit: "174530"
type: 1
wantsTranslation: false
 */
interface Problem {
	/**
	 * 题目 id
	 */
	val pid: String

	/**
	 * 题目难度
	 */
	val difficulty: Difficulty

	/**
	 * 题目标签
	 */
	val tags: List<LuoGuTag>

	/**
	 * 题目名称
	 */
	val name: String

	/**
	 * 通过数
	 */
	val totalAccepted: Long

	/**
	 * 提交数
	 */
	val totalSubmit: Long

	/**
	 * 不知道
	 * 可能是所属题库的 id
	 */
	val type: Int        //?

	/**
	 * 是否需要翻译
	 */
	val wantsTranslation: Boolean
}