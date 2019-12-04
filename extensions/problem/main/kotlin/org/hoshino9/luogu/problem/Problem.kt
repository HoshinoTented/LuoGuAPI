package org.hoshino9.luogu.problem

import com.google.gson.JsonArray
import com.google.gson.JsonObject
import org.hoshino9.luogu.IllegalStatusCodeException
import org.hoshino9.luogu.LuoGuUtils.baseUrl
import org.hoshino9.luogu.tag.IdLuoGuTag
import org.hoshino9.luogu.tag.LuoGuTag
import org.hoshino9.luogu.user.User
import org.hoshino9.luogu.utils.*

interface IBaseProblem {
	/**
	 * 难度
	 */
	val difficulty: Difficulty

	/**
	 * 题目 id
	 */
	val pid: String

	/**
	 * 题目标签
	 */
	val tags: List<LuoGuTag>

	/**
	 * 题目标题
	 */
	val title: String

	/**
	 * 题目总通过量
	 */
	val totalAccepted: Long

	/**
	 * 题目总提交量
	 */
	val totalSubmit: Long

	/**
	 * 题目所在题库类型
	 */
	val type: Type

	/**
	 * 题目是否需要翻译
	 */
	val wantsTranslation: Boolean
}

open class BaseProblem(private val source: JsonObject) : IBaseProblem {
	protected val data: JsonObject get() = source["currentData"].asJsonObject["problem"].asJsonObject
	protected val delegate = data.delegate

	override val pid: String by delegate

	override val difficulty: Difficulty
		get() = data["difficulty"].asInt.let {
			Difficulty.values()[it]
		}

	override val title: String by delegate

	override val tags: List<LuoGuTag>
		get() {
			return data["tags"].asJsonArray.map {
				IdLuoGuTag(it.asInt)
			}
		}

	override val type: Type
		get() {
			return Type.values().first { it.id == data["type"].asString }
		}

	override val totalAccepted: Long
		get() {
			return data["totalAccepted"].toString().toLong()
		}

	override val totalSubmit: Long
		get() {
			return data["totalSubmit"].toString().toLong()
		}

	override val wantsTranslation: Boolean by delegate

	override fun toString(): String {
		return data.toString()
	}
}


interface IProblem : IBaseProblem {
	/**
	 * 测试点限制
	 */
	data class Limit(val memory: Int, val time: Int)

	/**
	 * 输入输出样例
	 */
	data class Sample(val input: String, val output: String)

	/**
	 * 题目背景
	 * markdown 代码
	 */
	val background: String

	/**
	 * 是否可编辑
	 */
	val canEdit: Boolean

	/**
	 * 题目描述
	 * markdown 代码
	 */
	val description: String

	/**
	 * 提示
	 * markdown 代码
	 */
	val hint: String

	/**
	 * 测试点限制
	 */
	val limits: List<Limit>

	/**
	 * 输入格式
	 */
	val inputFormat: String

	/**
	 * 输出格式
	 */
	val outputFormat: String

	/**
	 * 题目提供者
	 */
	val provider: User

	/**
	 * 输入输出样例
	 */
	val samples: List<Sample>
}

open class Problem(source: JsonObject) : BaseProblem(source), IProblem {
	override val background: String by delegate

	override val canEdit: Boolean by delegate

	override val description: String by delegate

	override val hint: String by delegate

	override val inputFormat: String by delegate

	override val outputFormat: String by delegate

	override val limits: List<IProblem.Limit>
		get() {
			val json = data["limits"].asJsonObject
			val memory = json["memory"].asJsonArray
			val time = json["time"].asJsonArray

			return (0 until memory.size()).map {
				IProblem.Limit(memory[it].asInt, time[it].asInt)
			}
		}

	override val provider: User
		get() {
			return User(data["provider"].asJsonObject["uid"].asInt)
		}

	override val samples: List<IProblem.Sample>
		get() {
			return data["samples"].asJsonArray.map {
				it as JsonArray

				val `in` = it[0].asString
				val out = it[1].asString

				IProblem.Sample(`in`, out)
			}
		}
}