package org.hoshino9.luogu.record.listener

import okhttp3.Response
import okhttp3.WebSocket

typealias OnOpenType = (WebSocket, Response) -> Unit
typealias OnMessageType = (WebSocket, String) -> Unit