package org.hoshino9.luogu.record

import okhttp3.WebSocket
import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.record.listener.OnMessageType

interface Record {
	companion object {
		fun message(rid: String) = """{"type":"join_channel","channel":"record.track","channel_param":"$rid"}"""

		@JvmName("newInstance")
		operator fun invoke(rid: String): Record {
			return DefaultRecord(rid)
		}
	}

	val rid : String
	fun listen(client : LuoGu, listener : OnMessageType) : WebSocket
}