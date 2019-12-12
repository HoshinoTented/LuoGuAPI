package org.hoshino9.luogu.problem

import com.google.gson.JsonArray
import com.google.gson.JsonElement
import com.google.gson.JsonObject
import com.google.gson.JsonPrimitive
import org.hoshino9.luogu.IllegalStatusCodeException
import org.hoshino9.luogu.LuoGuUtils.baseUrl
import org.hoshino9.luogu.page.AbstractLuoGuPage
import org.hoshino9.luogu.tag.IdLuoGuTag
import org.hoshino9.luogu.tag.LuoGuTag
import org.hoshino9.luogu.user.BaseUser
import org.hoshino9.luogu.user.IBaseUser
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

open class BaseProblem(protected val source: JsonObject) : IBaseProblem {
	protected val delegate = source.delegate

	override val pid: String by delegate

	override val difficulty: Difficulty
		get() = source["difficulty"].asInt.let {
			Difficulty.values()[it]
		}

	override val title: String by delegate

	override val tags: List<LuoGuTag>
		get() {
			return source["tags"].asJsonArray.map {
				IdLuoGuTag(it.asInt)
			}
		}

	override val type: Type
		get() {
			return Type.values().first { it.id == source["type"].asString }
		}

	override val totalAccepted: Long
		get() {
			return source["totalAccepted"].run(::parseTotal)
		}

	override val totalSubmit: Long
		get() {
			return source["totalSubmit"].run(::parseTotal)
		}

	override val wantsTranslation: Boolean by delegate

	private fun parseTotal(elem: JsonElement): Long {
		elem as JsonPrimitive

		return when {
			elem.isString -> elem.asString.toLong()
			elem.isNumber -> elem.asLong
			else -> throw IllegalArgumentException(elem.toString())
		}
	}

	override fun toString(): String {
		return source.toString()
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
	val provider: IBaseUser

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
			val json = source["limits"].asJsonObject
			val memory = json["memory"].asJsonArray
			val time = json["time"].asJsonArray

			return (0 until memory.size()).map {
				IProblem.Limit(memory[it].asInt, time[it].asInt)
			}
		}

	override val provider: IBaseUser
		get() {
			return BaseUser(source["provider"].asJsonObject)
		}

	override val samples: List<IProblem.Sample>
		get() {
			return source["samples"].asJsonArray.map {
				it as JsonArray

				val `in` = it[0].asString
				val out = it[1].asString

				IProblem.Sample(`in`, out)
			}
		}
}

open class ProblemPage(val pid: String, client: HttpClient = emptyClient) : AbstractLuoGuPage(client) {
	override val url: String get() = "$baseUrl/problem/$pid"

	val problem: IProblem by lazy {
		Problem(currentData["problem"].asJsonObject)
	}
}