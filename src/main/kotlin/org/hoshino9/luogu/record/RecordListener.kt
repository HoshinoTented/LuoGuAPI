package org.hoshino9.luogu.record

import okhttp3.Response
import okhttp3.WebSocket
import okhttp3.WebSocketListener

typealias OnOpenType = (WebSocket, Response) -> Unit
typealias OnMessageType = (WebSocket, RecordResponse) -> Unit

abstract class AbstractRecordListener : WebSocketListener() {
	class Builder {
		private val socket = DefaultRecordListener()

		fun onMessage(closure : OnMessageType) : Builder = apply {
			socket.onMessage(closure)
		}

		fun onOpen(closure : OnOpenType) : Builder = apply {
			socket.onOpen(closure)
		}

		fun build() : AbstractRecordListener {
			return socket
		}
	}

	protected var onOpenClosure : OnOpenType = { _, _ -> Unit }
	protected var onMessageClosure : OnMessageType = { _, _ -> Unit }

	fun onMessage(closure : OnMessageType) {
		this.onMessageClosure = closure
	}

	fun onOpen(closure : OnOpenType) {
		this.onOpenClosure = closure
	}
}

open class DefaultRecordListener : AbstractRecordListener() {
	override fun onOpen(webSocket : WebSocket, response : Response) {
		onOpenClosure(webSocket, response)
	}

	override fun onMessage(webSocket : WebSocket, text : String) {
		onMessageClosure(webSocket, RecordResponse.Builder().json(text).build())
	}
}