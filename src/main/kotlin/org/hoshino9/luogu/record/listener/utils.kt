package org.hoshino9.luogu.record.listener

import okhttp3.Response
import okhttp3.WebSocket
import org.hoshino9.luogu.record.response.RecordResponse

typealias OnOpenType = (WebSocket, Response) -> Unit
typealias OnMessageType = (WebSocket, RecordResponse) -> Unit