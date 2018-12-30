package org.hoshino9.luogu.record.listener

import okhttp3.WebSocketListener

abstract class RecordListener : WebSocketListener() {
	class Builder {
		private val socket = DefaultRecordListener()

		fun onMessage(closure : OnMessageType) : Builder = apply {
			socket.onMessage(closure)
		}

		fun onOpen(closure : OnOpenType) : Builder = apply {
			socket.onOpen(closure)
		}

		fun build() : RecordListener {
			return socket
		}
	}

	protected var onOpenClosure : OnOpenType = { _, _ -> }
	protected var onMessageClosure : OnMessageType = { _, _ -> }

	fun onMessage(closure : OnMessageType) {
		this.onMessageClosure = closure
	}

	fun onOpen(closure : OnOpenType) {
		this.onOpenClosure = closure
	}
}

