package org.hoshino9.luogu.record.status

import com.google.gson.*
import com.google.gson.reflect.TypeToken
import org.hoshino9.luogu.globalGson
import org.hoshino9.luogu.record.TestCase
import java.lang.reflect.Type

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
		val successful : Boolean get() = flag == 12
	}

	data class SubTask(val judger : Int, val memory : Int, val score : Int, val status : Status, val time : Int)

	data class Detail(val testCases : List<TestCase>, val compileMessage : CompileMessage, val subTasks : List<SubTask>) {
		companion object Adapter : JsonDeserializer<Detail>, JsonSerializer<Detail> {
			override fun deserialize(json : JsonElement, typeOfT : Type?, context : JsonDeserializationContext) : Detail {
				return json.asJsonObject.run {
					val compileMessage = context.deserialize<CompileMessage>(this["compile"], CompileMessage::class.java)
					val subTasks = context.deserialize<List<SubTask>>(this["subtasks"], (object : TypeToken<List<SubTask>>() {}).type) ?: emptyList()
					val testCases = keySet().mapNotNull {
						if (it.startsWith("case")) {
							TestCase(context, it, this[it])
						} else null
					}

					Detail(testCases, compileMessage, subTasks)
				}
			}

			override fun serialize(src : Detail?, typeOfSrc : Type?, context : JsonSerializationContext) : JsonElement {
				src ?: return JsonNull.INSTANCE
				return JsonObject().apply {
					src.testCases.forEach {
						this.add(it.name, context.serialize(it))
					}

					this.add("compile", context.serialize(src.compileMessage))
					this.add("subtasks", context.serialize(src.subTasks))
				}
			}
		}
	}

	companion object {
		@JvmName("newInstance")
		operator fun invoke(json : String) : RecordStatus {
			return globalGson.fromJson(json, RecordStatusBean::class.java)
		}

		@JvmName("newInstance")
		operator fun invoke(elem : JsonElement) : RecordStatus {
			return globalGson.fromJson(elem, RecordStatusBean::class.java)
		}
	}

	val status : Status
	val memory : String
	val score : String
	val time : String
	val detail : Detail
}


