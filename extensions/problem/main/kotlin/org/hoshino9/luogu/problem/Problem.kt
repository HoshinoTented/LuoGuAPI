package org.hoshino9.luogu.problem

import com.google.gson.JsonArray
import com.google.gson.JsonObject
import org.hoshino9.luogu.IllegalStatusCodeException
import org.hoshino9.luogu.tag.IdLuoGuTag
import org.hoshino9.luogu.tag.LuoGuTag
import org.hoshino9.luogu.user.User
import org.hoshino9.luogu.utils.*

interface Problem {
	open class Factory(val source: JsonObject) : Problem {
		companion object {
			operator fun invoke(pid: String, client: HttpClient): Factory {
				val source = client.contentOnlyGet("https://www.luogu.org/problem/$pid") {
					it.assert()
					json(it.strData)
				}

				if (source["code"].asInt != 200) throw IllegalStatusCodeException(source["code"], source["currentData"].asJsonObject["errorMessage"])

				return Factory(source)
			}
		}

		val data: JsonObject get() = source["currentData"].asJsonObject.get("problem").asJsonObject
		val delegate = data.delegate

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

		override val type: Int by delegate

		override val totalAccepted: Long by delegate

		override val totalSubmit: Long by delegate

		override val wantsTranslation: Boolean by delegate

		override val background: String by delegate

		override val canEdit: Boolean by delegate

		override val description: String by delegate

		override val hint: String by delegate

		override val inputFormat: String by delegate

		override val outputFormat: String by delegate

		override val limits: List<Limit>
			get() {
				val json = data["limits"].asJsonObject
				val memory = json["memory"].asJsonArray
				val time = json["time"].asJsonArray

				return (0 until memory.size()).map {
					Limit(memory[it].asInt, time[it].asInt)
				}
			}

		override val provider: User
			get() {
				return User(data["provider"].asJsonObject["uid"].asInt.toString())
			}

		override val samples: List<Sample>
			get() {
				return data["samples"].asJsonArray.map {
					it as JsonArray

					val `in` = it[0].asString
					val out = it[1].asString

					Sample(`in`, out)
				}
			}

		fun newInstance(): Problem {
			return ProblemData(pid, difficulty, tags, title, totalAccepted, totalSubmit, type, wantsTranslation, background, canEdit, description, hint, limits, inputFormat, outputFormat, provider, samples)
		}

		override fun toString(): String {
			return data.toString()
		}
	}

	/**
	 * 测试点限制
	 */
	data class Limit(val memory: Int, val time: Int)

	/**
	 * 输入输出样例
	 */
	data class Sample(val input: String, val output: String)

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
	val title: String

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

data class ProblemData(override val pid: String, override val difficulty: Difficulty, override val tags: List<LuoGuTag>, override val title: String, override val totalAccepted: Long, override val totalSubmit: Long, override val type: Int, override val wantsTranslation: Boolean, override val background: String, override val canEdit: Boolean, override val description: String, override val hint: String, override val limits: List<Problem.Limit>, override val inputFormat: String, override val outputFormat: String, override val provider: User, override val samples: List<Problem.Sample>) : Problem