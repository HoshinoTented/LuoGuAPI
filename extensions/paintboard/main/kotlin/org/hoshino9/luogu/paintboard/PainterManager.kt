package org.hoshino9.luogu.paintboard

import kotlinx.coroutines.*
import java.util.LinkedList
import java.util.Queue
import kotlin.coroutines.CoroutineContext
import kotlin.coroutines.EmptyCoroutineContext

data class Timer(val painter: Painter, val queue: Queue<Timer>, val delay: Long) {
	private lateinit var timer: Job

	init {
		resetTimer()
	}

	fun resetTimer() {
		timer = GlobalScope.launch {
			delay(this@Timer.delay)

			queue.add(this@Timer)
		}
	}

	fun canPaint(): Boolean {
		return timer.isCompleted
	}
}

class PainterManager(val photoProvider: PhotoProvider, val begin: Pos, override val coroutineContext: CoroutineContext = EmptyCoroutineContext, val boardProvider: suspend () -> PaintBoard) : CoroutineScope {
	private val internalTimers: MutableList<Timer> = LinkedList()
	private val requestQueue: Queue<Timer> = LinkedList()

	fun paint(): Job {
		return launch {
			while (isActive) {
				if (requestQueue.isNotEmpty()) {
					val (pos, color) = photoProvider.current(begin)
					val cur = color

					if (cur == null) {
						println("Skip empty color: $pos")
						photoProvider.next()
						continue
					}

					if (boardProvider().board[pos] == cur) {
						println("Skip same color: $pos")
						photoProvider.next()
						continue
					}

					val front = requestQueue.remove()

					try {
						println("${front.painter.uid} is painting: $pos with color: $cur")
						val result = front.painter.paint(pos, cur)
						photoProvider.next()
						println("${front.painter.uid} is painted: $result")
					} catch (e: Exception) {
						println("${front.painter.uid} paint failed: ${e.message}")
					}

					front.resetTimer()
				}
			}
		}
	}

	fun add(painter: Painter, delay: Long) {
		internalTimers.add(Timer(painter, requestQueue, delay))
	}
}