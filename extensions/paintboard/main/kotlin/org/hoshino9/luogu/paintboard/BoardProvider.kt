package org.hoshino9.luogu.paintboard

import io.ktor.client.features.websocket.ws
import io.ktor.client.request.get
import io.ktor.http.cio.websocket.Frame
import io.ktor.http.cio.websocket.readText
import io.ktor.http.cio.websocket.send
import kotlinx.atomicfu.AtomicRef
import kotlinx.atomicfu.atomic
import kotlinx.coroutines.*
import kotlinx.coroutines.channels.Channel
import org.hoshino9.luogu.baseUrl
import org.hoshino9.luogu.utils.delegate
import org.hoshino9.luogu.utils.emptyClient
import org.hoshino9.luogu.utils.json
import java.io.PrintStream
import kotlin.coroutines.CoroutineContext
import kotlin.coroutines.EmptyCoroutineContext

interface BoardProvider {
	suspend fun board(): Board
}

object DefaultBoardProvider : BoardProvider {
	override suspend fun board(): Board {
		val lines = emptyClient.use { it.get<String>(boardApi).lines().dropLast(1) }
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

class WebSocketBoardProvider(override val coroutineContext: CoroutineContext = EmptyCoroutineContext) : BoardProvider, CoroutineScope {
	companion object {
		const val message = """{ "type": "join_channel", "channel": "paintboard", "channel_param": "" }"""
	}

	private var paintBoard: AtomicRef<Board?> = atomic(null)

	val job: Job

	init {
		job = launch {
			emptyClient.ws(boardWsApi) {
				send(message)

				for (frame in incoming) {
					if (frame is Frame.Text) {
						json(frame.readText()).delegate.let {
							val type: String by it

							if (type == "paintboard_update") {
								val x: Int by it
								val y: Int by it
								val color: Int by it

								val pos = Pos(x, y)

								paintBoard.value?.set(pos, color)
							} else if (type == "result") {
								paintBoard.value = DefaultBoardProvider.board()
							}

							Unit
						}
					}
				}
			}
		}
	}

	override suspend fun board(): Board {
		while (paintBoard.value == null) {
		}

		return paintBoard.value !!
	}
}