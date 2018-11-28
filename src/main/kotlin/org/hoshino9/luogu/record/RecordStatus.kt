package org.hoshino9.luogu.record

import com.google.gson.*
import com.google.gson.reflect.TypeToken
import org.hoshino9.luogu.globalGson
import java.lang.reflect.Type

object RecordStatusStatusAdapter : JsonSerializer<RecordStatus.Status>, JsonDeserializer<RecordStatus.Status> {
	override fun serialize(src : RecordStatus.Status?, typeOfSrc : Type?, context : JsonSerializationContext?) : JsonElement {
		return JsonPrimitive(src?.value ?: return JsonNull.INSTANCE)
	}

	override fun deserialize(json : JsonElement, typeOfT : Type?, context : JsonDeserializationContext?) : RecordStatus.Status {
		return json.run {
			RecordStatus.Status.values().first { it.value == asInt }
		}
	}
}

object RecordStatusAdapter : JsonDeserializer<RecordStatus> {
	override fun deserialize(json : JsonElement, typeOfT : Type?, context : JsonDeserializationContext?) : RecordStatus {
		return RecordStatus.Builder().json(json).build()
	}
}

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

	data class SubTask(val judger : Int, val memory : Int, val score : Int, val status : RecordStatus.Status, val time : Int)

	data class Detail(val testCases : List<TestCase>, val compileMessage : CompileMessage, val subTasks : List<SubTask>) {
		companion object Adapter : JsonDeserializer<Detail>, JsonSerializer<Detail> {
			override fun deserialize(json : JsonElement, typeOfT : Type?, context : JsonDeserializationContext) : Detail {
				return json.asJsonObject.run {
					val compileMessage = context.deserialize<CompileMessage>(this["compile"], CompileMessage::class.java)
					val subTasks = context.deserialize<List<SubTask>>(this["subtasks"], (object : TypeToken<List<SubTask>>() {}).type) ?: emptyList()
					val testCases = keySet().mapNotNull {
						if (it.startsWith("case")) {
							TestCase.Builder().context(context).name(it).json(this[it]).build()
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

	class Builder {
		private lateinit var mJson : String
		private lateinit var mElem : JsonElement

		fun json(json : String) = apply {
			this.mJson = json
		}

		fun json(elem : JsonElement) : Builder = apply {
			this.mElem = elem
		}

		fun build() : RecordStatus {
			return when {
				::mJson.isInitialized -> globalGson.fromJson(mJson, RecordStatusBean::class.java)
				::mElem.isInitialized -> globalGson.fromJson(mElem, RecordStatusBean::class.java)

				else -> throw NullPointerException("mJson or mElem == null")
			}
		}
	}

	val status : RecordStatus.Status
	val memory : String
	val score : String
	val time : String
	val detail : Detail
}

abstract class AbstractRecordStatus : RecordStatus

data class RecordStatusBean(
		override val status : RecordStatus.Status,
		override val memory : String,
		override val score : String,
		override val time : String,
		override val detail : RecordStatus.Detail
) : AbstractRecordStatus()
