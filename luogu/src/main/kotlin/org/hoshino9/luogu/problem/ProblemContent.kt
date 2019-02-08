package org.hoshino9.luogu.problem

import org.hoshino9.luogu.LuoGuUtils
import org.hoshino9.luogu.utils.*
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import org.jsoup.select.Elements

interface ProblemContent {
	companion object Parser {
		fun parse(id : String) : ProblemContent {
			return emptyClient.executeGet("${LuoGuUtils.baseUrl}/problemnew/show/$id") {
				it.assert()

				Jsoup.parse(it.strData).getElementsByClass("lg-article am-g").first() !!
			}.run { parse(id, this) }
		}

		fun parse(id : String, element : Element) : ProblemContent {
			fun condition(tag : String, text : String) : (Element) -> Boolean {
				return {
					(it.tagName() == tag && it.text() == text).not()
				}
			}

			val it = element.clone().children().iterator()

			val bg = it.takeWhile(condition("h2", "题目描述")).run(::Elements)
			val description = it.takeWhile(condition("h2", "输入输出格式")).run(::Elements)
			val inAndOutF = it.takeWhile(condition("h2", "输入输出样例")).run(::Elements)
			val inAndOut = it.takeWhile(condition("h2", "说明")).run(::Elements)
			val tip = it.takeWhile { true }.run(::Elements)

			return DefaultProblemContent(id, bg, description, inAndOutF, inAndOut, tip)
		}
	}

	/**
	 * 题目id
	 */
	val id : String

	/**
	 * 题目背景
	 */
	val background : Elements

	/**
	 * 题目描述
	 */
	val description : Elements

	/**
	 * 输入输出格式
	 */
	val inAndOutFormat : Elements

	/**
	 * 输入和输出的样例列表<br>
	 * Pair.first 是输入<br>
	 * Pair.second 是输出
	 */
	val inAndOut : Elements

	/**
	 * 题目提示
	 */
	val tip : Elements
}