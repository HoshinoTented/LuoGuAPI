import okhttp3.Request
import okhttp3.Response
import okhttp3.WebSocket
import okhttp3.WebSocketListener
import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.record.Record
import org.hoshino9.luogu.record.Solution
import org.hoshino9.luogu.record.listener.RecordListener
import org.hoshino9.luogu.record.postSolution
import org.hoshino9.luogu.utils.USER_AGENT

fun LuoGu.submit(): Record {
	return loggedUser.postSolution(Solution("P1001", Solution.Language.Haskell, """
main :: IO ()
main = do
  [a, b] <- (map read . words) <${'$'}> getLine

  print (a + b)
"""))
}

fun LuoGu.listen(record: Record) {
	record.listen(this) { _, msg ->
		//		if (msg.type != "heartbeat") {
//			msg.recordStatus.apply {
//				println("memory / time: $memory / $time")
//				println("all test case:")
//				detail.testCases.forEach {
//					println("""${it.title}: $score""")
//				}
//			}
//		} else println("heart beat")
		println(msg)
	}
}

fun main() {
	LuoGuTest().apply {
		loadCookie()
	}.luogu.apply {
		val record = submit().apply(::println)
		listen(record)
	}
}