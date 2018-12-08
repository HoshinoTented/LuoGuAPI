package org.hoshino9.luogu.problem

import org.jsoup.nodes.Element

interface ProblemContent {
	/**
	 * 题目id
	 */
	val id : String

	/**
	 * 题目背景
	 */
	val background : Element

	/**
	 * 题目描述
	 */
	val description : Element

	/**
	 * 输入格式
	 */
	val inputFormat : Element

	/**
	 * 输出格式
	 */
	val outputFormat : Element

	/**
	 * 输入和输出的样例列表<br>
	 * Pair.first 是输入<br>
	 * Pair.second 是输出
	 */
	val inAndOut : List<Pair<String, String>>

	/**
	 * 题目提示
	 */
	val tip : Element
}