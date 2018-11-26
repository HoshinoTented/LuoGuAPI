import org.hoshino9.luogu.record.RecordStatus
import org.hoshino9.luogu.record.TestCase
import org.junit.Test
import kotlin.test.assertEquals

class RecordTest {
	val recordJson = """{
  "detail" : {
    "case1" : {
      "desc" : "wrong answer On line 1 column 1, read W, expected 0.",
      "exit_code" : 0,
      "flag" : 6,
      "memory" : 800,
      "score" : 0,
      "signal" : 0,
      "subtask" : 0,
      "time" : 3
    },
    "case10" : {
      "desc" : "wrong answer On line 1 column 1, read W, expected 8.",
      "exit_code" : 0,
      "flag" : 6,
      "memory" : 804,
      "score" : 0,
      "signal" : 0,
      "subtask" : 0,
      "time" : 2
    },
    "case2" : {
      "desc" : "wrong answer On line 1 column 1, read W, expected 4.",
      "exit_code" : 0,
      "flag" : 6,
      "memory" : 684,
      "score" : 0,
      "signal" : 0,
      "subtask" : 0,
      "time" : 3
    },
    "case3" : {
      "desc" : "wrong answer On line 1 column 1, read W, expected 0.",
      "exit_code" : 0,
      "flag" : 6,
      "memory" : 824,
      "score" : 0,
      "signal" : 0,
      "subtask" : 0,
      "time" : 3
    },
    "case4" : {
      "desc" : "wrong answer On line 1 column 1, read W, expected 5.",
      "exit_code" : 0,
      "flag" : 6,
      "memory" : 940,
      "score" : 0,
      "signal" : 0,
      "subtask" : 0,
      "time" : 2
    },
    "case5" : {
      "desc" : "wrong answer On line 1 column 1, read W, expected 0.",
      "exit_code" : 0,
      "flag" : 6,
      "memory" : 508,
      "score" : 0,
      "signal" : 0,
      "subtask" : 0,
      "time" : 2
    },
    "case6" : {
      "desc" : "wrong answer On line 1 column 1, read W, expected 2.",
      "exit_code" : 0,
      "flag" : 6,
      "memory" : 672,
      "score" : 0,
      "signal" : 0,
      "subtask" : 0,
      "time" : 3
    },
    "case7" : {
      "desc" : "wrong answer On line 1 column 1, read W, expected 1.",
      "exit_code" : 0,
      "flag" : 6,
      "memory" : 812,
      "score" : 0,
      "signal" : 0,
      "subtask" : 0,
      "time" : 2
    },
    "case8" : {
      "desc" : "wrong answer On line 1 column 1, read W, expected 4.",
      "exit_code" : 0,
      "flag" : 6,
      "memory" : 804,
      "score" : 0,
      "signal" : 0,
      "subtask" : 0,
      "time" : 2
    },
    "case9" : {
      "desc" : "wrong answer On line 1 column 1, read W, expected 6.",
      "exit_code" : 0,
      "flag" : 6,
      "memory" : 808,
      "score" : 0,
      "signal" : 0,
      "subtask" : 0,
      "time" : 2
    },
    "compile" : {
      "content" : "",
      "flag" : 12
    },
    "final" : [],
    "finishedCaseCount" : 10,
    "init" : {
      "version" : "4"
    },
    "subtasks" : [
      {
        "judger" : 1,
        "memory" : 940,
        "score" : 0,
        "status" : 14,
        "time" : 24
      }
    ]
  },
  "memory" : "940",
  "score" : "0",
  "status" : 14,
  "time" : "24",
  "type" : "status_push"
}"""

	@Test
	fun deserialization() {
		val status = RecordStatus.Builder().rid("14149207").json(recordJson).build()
		status.run {
			assertEquals("940", memory)
			assertEquals("0", score)
			assertEquals(RecordStatus.Status.Unaccepted, this.status)
			assertEquals("24", time)

			detail.run {
				//compile
				assertEquals("", compileMessage.content)
				assertEquals(12, compileMessage.flag)
				assert(compileMessage.successful)

				//subtasks
				subTasks.first().run {
					assertEquals(1, judger)
					assertEquals(940, memory)
					assertEquals(0, score)
					assertEquals(RecordStatus.Status.Unaccepted, this.status)
					assertEquals(24, time)
				}

				testCases.first { it.name == "case9" }.run {
					assertEquals("wrong answer On line 1 column 1, read W, expected 6.", description)
					assertEquals(0, exitCode)
					assertEquals(TestCase.Status.WA, this.status)
					assertEquals(808, memory)
					assertEquals(0, score)
					assertEquals(0, signal)
					assertEquals(0, subTask)
					assertEquals(2, time)
				}
			}
		}
	}
}