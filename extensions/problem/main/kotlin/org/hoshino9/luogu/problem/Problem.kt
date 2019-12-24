package org.hoshino9.luogu.problem

import com.google.gson.*
import com.google.gson.annotations.JsonAdapter
import org.hoshino9.luogu.LuoGuUtils.baseUrl
import org.hoshino9.luogu.page.AbstractLuoGuPage
import org.hoshino9.luogu.tag.IdLuoGuTag
import org.hoshino9.luogu.tag.LuoGuTag
import org.hoshino9.luogu.user.BaseUser
import org.hoshino9.luogu.user.IBaseUser
import org.hoshino9.luogu.utils.*

@JsonAdapter(BaseProblem.Serializer::class)
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

data class BaseProblem(override val difficulty: Difficulty, override val pid: String, override val tags: List<LuoGuTag>, override val title: String, override val totalAccepted: Long, override val totalSubmit: Long, override val type: Type, override val wantsTranslation: Boolean) : IBaseProblem {
	companion object Serializer : Deserializable<IBaseProblem>(IBaseProblem::class), JsonDeserializer<IBaseProblem> {
		override fun deserialize(json: JsonElement, typeOfT: java.lang.reflect.Type, context: JsonDeserializationContext): IBaseProblem {
			fun parseTotal(elem: JsonElement): Long {
				elem as JsonPrimitive

				return when {
					elem.isString -> elem.asString.toLong()
					elem.isNumber -> elem.asLong
					else -> throw IllegalArgumentException(elem.toString())
				}
			}

			val source = json.asJsonObject
			val provider = source.provider

			val pid: String by provider
			val difficulty: Difficulty = Difficulty.values()[source["difficulty"].asInt]
			val title: String by provider
			val tags: List<LuoGuTag> = source["tags"].asJsonArray.map {
				IdLuoGuTag(it.asInt)
			}

			val type: Type = Type.values().first { it.id == source["type"].asString }
			val totalAccepted: Long = source["totalAccepted"].run(::parseTotal)
			val totalSubmit: Long = source["totalSubmit"].run(::parseTotal)
			val wantsTranslation: Boolean by provider

			return BaseProblem(difficulty, pid, tags, title, totalAccepted, totalSubmit, type, wantsTranslation)
		}
	}
}

@JsonAdapter(Problem.Serializer::class)
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

data class Problem(override val background: String, override val canEdit: Boolean, override val description: String, override val hint: String, override val limits: List<IProblem.Limit>, override val inputFormat: String, override val outputFormat: String, override val provider: IBaseUser, override val samples: List<IProblem.Sample>, private val baseProblem: IBaseProblem) : IBaseProblem by baseProblem, IProblem {
	companion object Serializer : Deserializable<IProblem>(IProblem::class), JsonDeserializer<IProblem> {
		override fun deserialize(json: JsonElement, typeOfT: java.lang.reflect.Type, context: JsonDeserializationContext): IProblem {
			val source = json.asJsonObject
			val jsonProvider = source.provider

			val background: String by jsonProvider
			val canEdit: Boolean by jsonProvider
			val description: String by jsonProvider
			val hint: String by jsonProvider
			val inputFormat: String by jsonProvider
			val outputFormat: String by jsonProvider

			val limits: List<IProblem.Limit> = run {
				val json = source["limits"].asJsonObject
				val memory = json["memory"].asJsonArray
				val time = json["time"].asJsonArray

				(0 until memory.size()).map {
					IProblem.Limit(memory[it].asInt, time[it].asInt)
				}
			}

			val provider: IBaseUser = BaseUser(source["provider"].asJsonObject)
			val samples: List<IProblem.Sample> = source["samples"].asJsonArray.map {
				it.asJsonArray.let {
					val `in` = it[0].asString
					val out = it[1].asString

					IProblem.Sample(`in`, out)
				}
			}

			val baseProblem: IBaseProblem = context.deserialize(json, IBaseProblem::class.java)

			return Problem(background, canEdit, description, hint, limits, inputFormat, outputFormat, provider, samples, baseProblem)
		}
	}
}

open class ProblemPage(val pid: String, client: HttpClient = emptyClient) : AbstractLuoGuPage(client) {
	override val url: String get() = "$baseUrl/problem/$pid"

	val problem: IProblem by lazy {
		Problem(currentData["problem"].asJsonObject)
	}
}