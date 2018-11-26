package org.hoshino9.luogu.record

import com.google.gson.*
import com.google.gson.reflect.TypeToken
import org.hoshino9.luogu.globalGson
import java.lang.reflect.Type

object RecordStatusAdapter : JsonSerializer<RecordStatus.Status>, JsonDeserializer<RecordStatus.Status> {
	override fun serialize(src : RecordStatus.Status?, typeOfSrc : Type?, context : JsonSerializationContext?) : JsonElement {
		return JsonPrimitive(src?.value ?: return JsonNull.INSTANCE)
	}

	override fun deserialize(json : JsonElement, typeOfT : Type?, context : JsonDeserializationContext?) : RecordStatus.Status {
		return json.run {
			RecordStatus.Status.values().first { it.value == asInt }
		}
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
		private lateinit var mRid : String
		private lateinit var mJson : String

		fun rid(rid : String) = apply {
			this.mRid = rid
		}

		fun json(json : String) = apply {
			this.mJson = json
		}

		fun build() : RecordStatus {
			return globalGson.fromJson(mJson, RecordStatusBeen::class.java).apply {
				this.rid = mRid
			}
		}
	}

	val rid : String
	val status : RecordStatus.Status
	val memory : String
	val score : String
	val time : String
	val detail : Detail
}

abstract class AbstractRecordStatus : RecordStatus {
	override fun equals(other : Any?) : Boolean {
		if (this === other) return true
		if (other !is AbstractRecordStatus) return false

		return other.rid == rid
	}

	override fun hashCode() : Int {
		return rid.hashCode()
	}

	override fun toString() : String {
		return rid
	}
}

data class RecordStatusBeen(
		override val status : RecordStatus.Status,
		override val memory : String,
		override val score : String,
		override val time : String,
		override val detail : RecordStatus.Detail
) : AbstractRecordStatus() {
	override lateinit var rid : String
}
