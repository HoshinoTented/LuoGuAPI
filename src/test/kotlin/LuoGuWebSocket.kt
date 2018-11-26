import okhttp3.Request
import okhttp3.Response
import okhttp3.WebSocket
import okhttp3.WebSocketListener
import okhttp3.internal.ws.RealWebSocket
import okio.ByteString
import org.hoshino9.luogu.USER_AGENT
import org.hoshino9.luogu.data
import org.hoshino9.luogu.defaultClient
import org.hoshino9.luogu.emptyClient

const val message = """{"type":"join_channel","channel":"record.track","channel_param":"14149207"}"""

fun main(args : Array<String>) {
	val ws = defaultClient.newWebSocket(Request.Builder().url("wss://ws.luogu.org/ws").addHeader("User-Agent", USER_AGENT).addHeader("Cookie", "__client_id=${LuoGuLoginTest.config["__client_id"]}").build(), object : WebSocketListener() {
		override fun onOpen(webSocket : WebSocket, response : Response) {
			println("opened")
			println("request headers: ")
			println(response.request().headers())
			println("response headers: ")
			println(response.headers())
			webSocket.send(message)
			println("sent")
		}

		override fun onMessage(webSocket : WebSocket, text : String) {
			println(text)
		}

		override fun onMessage(webSocket : WebSocket, bytes : ByteString) {
			println(bytes)
		}
	})
}