package org.hoshino9.luogu.record

import io.ktor.client.features.websocket.ws
import io.ktor.http.cio.websocket.readBytes
import io.ktor.http.cio.websocket.send
import okhttp3.OkHttpClient
import okhttp3.Request
import okhttp3.WebSocket
import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.record.listener.OnMessageType
import org.hoshino9.luogu.record.listener.RecordListener
import org.hoshino9.luogu.utils.USER_AGENT

abstract class AbstractRecord : Record {
	override fun listen(client : LuoGu, listener : OnMessageType) : WebSocket {
		return OkHttpClient().newWebSocket(
				Request.Builder()
						.url("wss://ws.luogu.org/ws")
						.addHeader("User-Agent", USER_AGENT)
						.addHeader("Cookie", "__client_id=${client.clientId}; _uid=${client.uid}")
						.build(),
				RecordListener.Builder().onOpen { socket, _ ->
					socket.send(Record.message(rid))
				}.onMessage(listener).build())
	}


	override fun toString() : String {
		return rid
	}

	override fun equals(other : Any?) : Boolean {
		if (this === other) return true
		if (other !is AbstractRecord) return false
		return rid == other.rid
	}

	override fun hashCode() : Int {
		return rid.hashCode()
	}
}