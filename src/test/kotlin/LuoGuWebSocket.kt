import org.hoshino9.luogu.record.Record

const val message = """{"type":"join_channel","channel":"record.track","channel_param":"14140253"}"""

fun main(args : Array<String>) {
	LuoGuTest().apply {
		loadCookie()
	}.luogu.apply {
		val ws = Record("14140253").listen(this) { _, msg ->
			if (msg.type != "heartbeat") {
				msg.recordStatus.apply {
					println("memory / time: $memory / $time")
					println("all test case:")
					detail.testCases.forEach {
						println("""${it.name}: $score""")
					}
				}
			} else println("heart beat")
		}
	}
}