import okhttp3.Request
import okhttp3.Response
import okhttp3.WebSocket
import okhttp3.WebSocketListener
import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.USER_AGENT
import org.hoshino9.luogu.defaultClient
import org.hoshino9.luogu.record.Record
import org.hoshino9.okhttp.LuoGuOnlyCookieJar

const val message = """{"type":"join_channel","channel":"record.track","channel_param":"14140253"}"""

fun main(args : Array<String>) {
//	val ws = LuoGuTest().apply {
//		loadCookie()
//	}.luogu.apply {
//		client.newWebSocket(
//				Request.Builder()
//						.url("wss://ws.luogu.org/ws")
//						.addHeader("User-Agent", USER_AGENT)
//						.addHeader("Cookie", "__client_id=$clientId")
//						.build(),
//				object : WebSocketListener() {
//					override fun onOpen(webSocket : WebSocket, response : Response) {
//						println("opened")
//						println("request headers: ")
//						println(response.request().headers())
//						println("response headers: ")
//						println(response.headers())
//						webSocket.send(message)
//						println("sent")
//					}
//
//					override fun onMessage(webSocket : WebSocket, text : String) {
//						println(text)
//					}
//				})
//	}
//
//	println("Hello world!")

	LuoGuTest().apply {
		loadCookie()
	}.luogu.apply {
		Record.Builder().recordId("14140253").build().listen(this) { _, msg ->
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