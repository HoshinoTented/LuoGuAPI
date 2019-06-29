package org.hoshino9.luogu.record

import com.google.gson.JsonObject
import org.hoshino9.luogu.utils.delegate

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
			return invoke(name, org.hoshino9.luogu.utils.json(json))
		}

		@JvmName("newInstance")
		operator fun invoke(name: String, elem: JsonObject): TestCase {
			return elem.delegate.let {
				val desc : String by it
				val exit_code : Int by it
				val flag : Int by it
				val subtask : Int by it
				val memory : Int by it
				val score : Int by it
				val signal : Int by it
				val time : Int by it

				TestCaseBean(
						desc, exit_code,
						Status.values().first { it.value == flag },
						subtask, memory, score, signal, time
				).apply {
					this.name = name
				}
			}
		}
	}
}