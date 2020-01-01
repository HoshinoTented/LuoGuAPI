package org.hoshino9.luogu.paintboard

import io.ktor.client.features.websocket.ws
import io.ktor.client.request.get
import io.ktor.http.cio.websocket.Frame
import io.ktor.http.cio.websocket.readText
import io.ktor.http.cio.websocket.send
import kotlinx.coroutines.*
import org.hoshino9.luogu.LuoGuUtils
import org.hoshino9.luogu.LuoGuUtils.baseUrl
import org.hoshino9.luogu.utils.delegate
import org.hoshino9.luogu.utils.emptyClient
import org.hoshino9.luogu.utils.json
import java.io.PrintStream

interface BoardProvider {
	suspend fun board(): Board
}

class DefaultBoardProvider(val url: String = "$baseUrl/paintBoard/board") : BoardProvider {
	override suspend fun board(): Board {
		val lines = emptyClient.get<String>(url).lines().dropLast(1)
		val board = Board(400, 800)

		lines.forEachIndexed { x, line ->
			line.forEachIndexed { y, color ->
				val index = color.toString().toInt(32)
				board.board[x][y] = index
			}
		}

		return board
	}
}

class WebSocketBoardProvider(val wsUrl: String = "wss://ws.luogu.com.cn/ws", val boardUrl: String = "$baseUrl/paintBoard/board", val scope: CoroutineScope = GlobalScope) : BoardProvider {
	companion object {
		const val message = """{ "type": "join_channel", "channel": "paintboard", "channel_param": "" }"""
	}

	private val paintBoard: Board = runBlocking(scope.coroutineContext) {
		DefaultBoardProvider(boardUrl).board()
	}

	val job: Job

	init {
		job = scope.launch {
			emptyClient.ws(wsUrl) {
				send(message)

				for (frame in incoming) {
					if (frame is Frame.Text) {
						json(frame.readText()).delegate.let {
							val _ws_type: String by it

							if (_ws_type == "server_broadcast") {
								val x: Int by it
								val y: Int by it
								val color: Int by it

								val pos = Pos(x, y)

								paintBoard[pos] = color
							}
						}
					}
				}
			}
		}
	}

	override suspend fun board(): Board {
		return paintBoard
	}
}