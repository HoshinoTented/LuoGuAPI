package org.hoshino9.luogu.record

import com.google.gson.*
import com.google.gson.annotations.SerializedName
import org.hoshino9.luogu.globalGson
import java.lang.reflect.Type

object TestCaseStatusAdapter : JsonSerializer<TestCase.Status>, JsonDeserializer<TestCase.Status> {
	override fun serialize(src : TestCase.Status?, typeOfSrc : Type?, context : JsonSerializationContext?) : JsonElement {
		return JsonPrimitive(src?.value ?: return JsonNull.INSTANCE)
	}

	override fun deserialize(json : JsonElement?, typeOfT : Type?, context : JsonDeserializationContext?) : TestCase.Status {
		return TestCase.Status.values().first { it.value == json !!.asInt }
	}
}

interface TestCase {
	enum class Status(val value : Int) {
		AC(12),
		OLE(3),
		MLE(4),
		TLE(5),
		WA(6),
		RE(7)
	}

	val name : String
	val description : String
	val exitCode : Int
	val status : Status
	val memory : Int
	val score : Int
	val signal : Int
	val subTask : Int
	val time : Int

	companion object {
		@JvmName("newInstance")
		operator fun invoke(name : String, json : String) : TestCase {
			return globalGson.fromJson<TestCaseBean>(json, TestCaseBean::class.java).apply {
				this.name = name
			}
		}

		@JvmName("newInstance")
		operator fun invoke(name : String, elem : JsonElement) : TestCase {
			return globalGson.fromJson<TestCaseBean>(elem, TestCaseBean::class.java).apply {
				this.name = name
			}
		}

		@JvmName("newInstance")
		operator fun invoke(context : JsonDeserializationContext, name : String, elem : JsonElement) : TestCase {
			return context.deserialize<TestCaseBean>(elem, TestCaseBean::class.java).apply {
				this.name = name
			}
		}
	}
}

abstract class AbstractTestCase : TestCase {
	override fun toString() : String {
		//language=JSON
		return """{
  "name" : "$name",
  "desc" : "$description",
  "exit_code" : $exitCode,
  "flag" : ${status.value},
  "memory" : $memory,
  "score" : $score,
  "signal" : $signal,
  "subtask" : $subTask,
  "time" : $time
}"""
	}
}

data class TestCaseBean(
		@SerializedName("desc") override val description : String,
		@SerializedName("exit_code") override val exitCode : Int,
		@SerializedName("flag") override val status : TestCase.Status,
		@SerializedName("subtask") override val subTask : Int,
		override val memory : Int,
		override val score : Int,
		override val signal : Int,
		override val time : Int
) : AbstractTestCase() {
	override lateinit var name : String
}