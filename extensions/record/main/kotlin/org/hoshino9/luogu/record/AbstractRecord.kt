package org.hoshino9.luogu.record

import io.ktor.client.features.websocket.ws
import io.ktor.http.cio.websocket.*
import org.hoshino9.luogu.LuoGu

abstract class AbstractRecord : Record {
	override suspend fun listen(client: LuoGu, listener: suspend WebSocketSession.() -> Unit) {
		client.client.ws("wss://ws.luogu.com.cn/ws") {
			send(Record.message(rid))
			listener()
		}
	}


	override fun toString(): String {
		return rid
	}

	override fun equals(other: Any?): Boolean {
		if (this === other) return true
		if (other !is AbstractRecord) return false
		return rid == other.rid
	}

	override fun hashCode() : Int {
		return rid.hashCode()
	}
}