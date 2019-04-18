@file:Suppress("MemberVisibilityCanBePrivate")

package org.hoshino9.luogu.problem

import org.hoshino9.luogu.color.*
import org.hoshino9.luogu.tag.ColoredLuoGuTag
import org.jsoup.nodes.Element

interface Problem {
	sealed class Difficulty(text: String, color: LuoGuColor) : ColoredLuoGuTag(text, - 1, color.toColor()) {
		companion object {
			val difficulties = arrayOf(Red, Orange, Yellow, Green, Blue, Purple, Black, Unknown)

			operator fun invoke(elem : Element) : Difficulty {
				(elem.luoguBadgeColor ?: throw IllegalArgumentException("Color not found ${elem.classNames()}")).let { color ->
					return difficulties.first { it.color == color.toColor() }
				}
			}
		}

		object Red : Difficulty("入门难度", LuoGuColor.Red)
		object Orange : Difficulty("普及-", LuoGuColor.Orange)
		object Yellow : Difficulty("普及/提高-", LuoGuColor.Yellow)
		object Green : Difficulty("普及+/提高", LuoGuColor.Green)
		object Blue : Difficulty("提高+/省选-", LuoGuColor.BlueLight)
		object Purple : Difficulty("省选/NOI-", LuoGuColor.Purple)
		object Black : Difficulty("NOI/NOI+/CTSC", LuoGuColor.BlueDark)
		object Unknown : Difficulty("尚无评定", LuoGuColor.Gray)
	}

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
	val tags: List<ColoredLuoGuTag>

	/**
	 * 题目内容
	 */
	val content : ProblemContent
}