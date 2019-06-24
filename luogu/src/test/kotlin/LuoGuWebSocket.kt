import okhttp3.Request
import okhttp3.Response
import okhttp3.WebSocket
import okhttp3.WebSocketListener
import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.problem.Solution
import org.hoshino9.luogu.record.Record
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
//					println("""${it.name}: $score""")
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
//		Thread.sleep(5000)
		listen(record)

//		client.newWebSocket(
//				Request.Builder()
//						.url("wss://ws.luogu.org/ws")
//						.addHeader("User-Agent", USER_AGENT)
//						.addHeader("Cookie", "_uid=124166; __client_id=8a1d754e1d6fdc39da4954af9ab275588eafb396")
//						.build(), object : WebSocketListener() {
//			override fun onOpen(webSocket: WebSocket, response: Response) {
//				webSocket.send("""{
//                "type": "join_channel",
//                "channel": "record.track",
//                "channel_param": "20009321"
//            }""")
//			}
//
//			override fun onMessage(webSocket: WebSocket, text: String) {
//				println(text)
//			}
//		})
	}
}