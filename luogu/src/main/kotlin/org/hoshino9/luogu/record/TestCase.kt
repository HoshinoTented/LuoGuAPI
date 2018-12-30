package org.hoshino9.luogu.record

import com.google.gson.*
import org.hoshino9.luogu.utils.globalGson

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