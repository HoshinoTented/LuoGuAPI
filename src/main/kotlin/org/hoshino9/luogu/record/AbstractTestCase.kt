package org.hoshino9.luogu.record

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