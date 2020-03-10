package org.hoshino9.luogu.problem

import com.google.gson.*
import com.google.gson.annotations.JsonAdapter
import org.hoshino9.luogu.baseUrl
import org.hoshino9.luogu.page.AbstractLuoGuPage
import org.hoshino9.luogu.page.Builder
import org.hoshino9.luogu.tag.IdLuoGuTag
import org.hoshino9.luogu.tag.LuoGuTag
import org.hoshino9.luogu.user.BaseUserImpl
import org.hoshino9.luogu.user.BaseUser
import org.hoshino9.luogu.utils.*

@JsonAdapter(BaseProblemImpl.Serializer::class)
interface BaseProblem {
	companion object;
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

	/**
	 * 题目满分
	 */
	val fullScore: Int
}

data class BaseProblemImpl(override val difficulty: Difficulty, override val pid: String, override val tags: List<LuoGuTag>, override val title: String, override val totalAccepted: Long, override val totalSubmit: Long, override val type: Type, override val wantsTranslation: Boolean, override val fullScore: Int) : BaseProblem {
	companion object Serializer : Deserializable<BaseProblem>(BaseProblem::class), JsonDeserializer<BaseProblem> {
		override fun deserialize(json: JsonElement, typeOfT: java.lang.reflect.Type, context: JsonDeserializationContext): BaseProblem {
			fun parseTotal(elem: JsonElement): Long {
				elem as JsonPrimitive

				return when {
					elem.isString -> elem.asString.toLong()
					elem.isNumber -> elem.asLong
					else -> throw IllegalArgumentException(elem.toString())
				}
			}

			val source = json.asJsonObject
			val delegate = source.delegate

			val pid: String by delegate
			val difficulty: Difficulty = Difficulty.values()[source["difficulty"].asInt]
			val title: String by delegate
			val tags: List<LuoGuTag> = source["tags"].asJsonArray.map {
				IdLuoGuTag(it.asInt)
			}

			val type: Type = Type.values().first { it.id == source["type"].asString }
			val totalAccepted: Long = source["totalAccepted"].run(::parseTotal)
			val totalSubmit: Long = source["totalSubmit"].run(::parseTotal)
			val wantsTranslation: Boolean by delegate
			val fullScore: Int by delegate

			return BaseProblemImpl(difficulty, pid, tags, title, totalAccepted, totalSubmit, type, wantsTranslation, fullScore)
		}
	}
}

@JsonAdapter(ProblemImpl.Serializer::class)
interface Problem : BaseProblem {
	companion object;
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
	val provider: BaseUser

	/**
	 * 输入输出样例
	 */
	val samples: List<Sample>
}

data class ProblemImpl(override val background: String, override val canEdit: Boolean, override val description: String, override val hint: String, override val limits: List<Problem.Limit>, override val inputFormat: String, override val outputFormat: String, override val provider: BaseUser, override val samples: List<Problem.Sample>, private val baseProblem: BaseProblem) : BaseProblem by baseProblem, Problem {
	companion object Serializer : Deserializable<Problem>(Problem::class), JsonDeserializer<Problem> {
		override fun deserialize(json: JsonElement, typeOfT: java.lang.reflect.Type, context: JsonDeserializationContext): Problem {
			val source = json.asJsonObject
			val jsonDelegate = source.delegate

			val background: String by jsonDelegate
			val canEdit: Boolean by jsonDelegate
			val description: String by jsonDelegate
			val hint: String by jsonDelegate
			val inputFormat: String by jsonDelegate
			val outputFormat: String by jsonDelegate

			val limits: List<Problem.Limit> = run {
				val json = source["limits"].asJsonObject
				val memory = json["memory"].asJsonArray
				val time = json["time"].asJsonArray

				(0 until memory.size()).map {
					Problem.Limit(memory[it].asInt, time[it].asInt)
				}
			}

			val delegate: BaseUser = BaseUserImpl(source["provider"].asJsonObject)
			val samples: List<Problem.Sample> = source["samples"].asJsonArray.map {
				it.asJsonArray.let {
					val `in` = it[0].asString
					val out = it[1].asString

					Problem.Sample(`in`, out)
				}
			}

			val baseProblem: BaseProblem = context.deserialize(json, BaseProblem::class.java)

			return ProblemImpl(background, canEdit, description, hint, limits, inputFormat, outputFormat, delegate, samples, baseProblem)
		}
	}
}

@JsonAdapter(ProblemPageImpl.Serializer::class)
interface ProblemPage {
	val problem: Problem
}

data class ProblemPageImpl(override val problem: Problem) : ProblemPage {
	companion object Serializer : Deserializable<ProblemPage>(ProblemPage::class), JsonDeserializer<ProblemPage> {
		override fun deserialize(json: JsonElement, typeOfT: java.lang.reflect.Type, context: JsonDeserializationContext): ProblemPage = run {
			val data = json.asJsonObject.delegate
			val problem: JsonObject by data

			ProblemPageImpl(ProblemImpl(problem))
		}
	}
}

class ProblemPageBuilder(val pid: String, client: HttpClient = emptyClient) : AbstractLuoGuPage(client), Builder<ProblemPage> {
	override val url: String get() = "$baseUrl/problem/$pid"

	override fun build(): ProblemPage = run {
		ProblemPageImpl(currentData)
	}
}