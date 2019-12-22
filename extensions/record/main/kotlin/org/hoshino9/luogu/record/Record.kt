package org.hoshino9.luogu.record

import io.ktor.http.cio.websocket.WebSocketSession
import okhttp3.WebSocket
import org.hoshino9.luogu.LuoGu

interface Record {
	companion object {
		fun message(rid: String) = """{"type":"join_channel","channel":"record.track","channel_param":"$rid"}"""

		@JvmName("newInstance")
		operator fun invoke(rid: String): Record {
			return DefaultRecord(rid)
		}
	}

	val rid: String
	suspend fun listen(client: LuoGu, listener: suspend WebSocketSession.() -> Unit)
}