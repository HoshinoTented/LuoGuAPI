package org.hoshino9.luogu.record

import okhttp3.Request
import okhttp3.WebSocket
import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.USER_AGENT

interface Record {
	companion object {
		internal fun message(rid : String) = """{"type":"join_channel","channel":"record.track","channel_param":"$rid"}"""

		@JvmName("newInstance")
		operator fun invoke(rid : String) : Record {
			return DefaultRecord(rid)
		}
	}

	val rid : String
	fun listen(client : LuoGu, listener : OnMessageType) : WebSocket
}

abstract class AbstractRecord : Record {
	override fun listen(client : LuoGu, listener : OnMessageType) : WebSocket {
		return client.client.newWebSocket(
				Request.Builder()
						.url("wss://ws.luogu.org/ws")
						.addHeader("User-Agent", USER_AGENT)
						.addHeader("Cookie", "__client_id=${client.clientId}")
						.build(),
				AbstractRecordListener.Builder().onOpen { socket, _ ->
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

open class DefaultRecord(override val rid : String) : AbstractRecord()