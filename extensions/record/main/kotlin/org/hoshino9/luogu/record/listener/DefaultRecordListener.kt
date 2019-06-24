package org.hoshino9.luogu.record.listener

import okhttp3.Response
import okhttp3.WebSocket
import org.hoshino9.luogu.record.response.RecordResponse

open class DefaultRecordListener : RecordListener() {
	override fun onOpen(webSocket : WebSocket, response : Response) {
		onOpenClosure(webSocket, response)
	}

	override fun onMessage(webSocket : WebSocket, text : String) {
		onMessageClosure(webSocket, text)
	}
}