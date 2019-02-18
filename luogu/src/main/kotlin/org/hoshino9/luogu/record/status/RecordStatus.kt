package org.hoshino9.luogu.record.status

import org.hoshino9.luogu.record.TestCase
import org.hoshino9.luogu.utils.delegate
import org.json.JSONArray
import org.json.JSONObject

interface RecordStatus {
	enum class Status(val value : Int) {
		Waiting(0),
		Judging(1),
		CompileError(2),
		Accepted(12),
		Unaccepted(14),
		HackSuccess(21),
		HackFailure(22),
		HackSkipped(23)
	}

	data class CompileMessage(val content : String, val flag : Int) {
		companion object {
			operator fun invoke(obj : JSONObject) : CompileMessage {
				return obj.delegate.let {
					val content : String by it
					val flag : Int by it

					CompileMessage(content, flag)
				}
			}
		}

		val successful : Boolean get() = flag == 12
	}

	data class SubTask(val judger : Int, val memory : Int, val score : Int, val status : Status, val time : Int) {
		companion object {
			operator fun invoke(obj : JSONObject) : SubTask {
				return obj.delegate.let {
					val judger : Int by it
					val memory : Int by it
					val score : Int by it
					val status : Int by it
					val time : Int by it

					SubTask(judger, memory, score, Status.values().first { it.value == status }, time)
				}
			}
		}
	}

	data class Detail(val testCases : List<TestCase>, val compileMessage : CompileMessage, val subTasks : List<SubTask>) {
		companion object {
			operator fun invoke(obj : JSONObject) : Detail {
				return obj.delegate.let {
					val compile : JSONObject by it
					val testCases = it.obj.keySet().mapNotNull { name ->
						name.takeIf { it.startsWith("case") }?.run {
							TestCase(name, it.obj.getJSONObject(name))
						}
					}

					val subtasks : JSONArray? by it

					Detail(testCases, compile.run(CompileMessage.Companion::invoke),
							subtasks?.map {
								SubTask(it as JSONObject)
							} ?: emptyList())
				}
			}
		}
	}

	companion object {
		@JvmName("newInstance")
		operator fun invoke(json : String) : RecordStatus {
			return invoke(JSONObject(json))
		}

		@JvmName("newInstance")
		operator fun invoke(elem : JSONObject) : RecordStatus {
			return elem.delegate.let {
				val status : Int by it
				val memory : String by it
				val score : String by it
				val time : String by it
				val detail : JSONObject by it

				RecordStatusBean(
						Status.values().first { it.value == status },
						memory, score, time, Detail(detail))
			}
		}
	}

	val status : Status
	val memory : String
	val score : String
	val time : String
	val detail : Detail
}


