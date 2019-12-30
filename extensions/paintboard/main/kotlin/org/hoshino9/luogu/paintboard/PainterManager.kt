package org.hoshino9.luogu.paintboard

import kotlinx.coroutines.*
import java.util.LinkedList
import java.util.Queue
import kotlin.coroutines.CoroutineContext
import kotlin.coroutines.EmptyCoroutineContext

data class Timer(val painter: Painter, val queue: Queue<Timer>, val scope: CoroutineScope = GlobalScope, val delay: Long) {
	private lateinit var timer: Job

	init {
		resetTimer()
	}

	fun resetTimer() {
		timer = scope.launch {
			delay(this@Timer.delay)

			queue.add(this@Timer)
		}
	}

	fun canPaint(): Boolean {
		return timer.isCompleted
	}
}

class PainterManager(val photoProvider: PhotoProvider, val begin: Pos, override val coroutineContext: CoroutineContext = EmptyCoroutineContext, val boardProvider: suspend () -> Board) : CoroutineScope {
	private val internalTimers: MutableList<Timer> = LinkedList()
	private val requestQueue: Queue<Timer> = LinkedList()

	val timers: List<Timer> get() = internalTimers

	fun paint(): Job {
		return launch {
			while (isActive) {
				if (requestQueue.isNotEmpty()) {
					val (pos, color) = photoProvider.current()
					val currentPos = Pos(begin.x + pos.x, begin.y + pos.y)

					if (color == null) {
						println("Skip empty color: $currentPos(offset: $pos)")
						photoProvider.next()
						continue
					}

					if (boardProvider()[pos] == color) {
						println("Skip same color: $currentPos(offset: $pos)")
						photoProvider.next()
						continue
					}

					val current = requestQueue.remove()

					try {
						println("${current.painter.uid} is painting: $currentPos(offset: $pos) with color: $color")

						val result = current.painter.paint(pos, color)

						photoProvider.next()

						println("${current.painter.uid} is painted: $result")
					} catch (e: Exception) {
						println("${current.painter.uid} paint failed: ${e.message}")

						if (e.message == "没有登录") {
							println("removed no login: ${current.painter.uid}")
							internalTimers.remove(current)

							continue
						}
					}

					current.resetTimer()
				}
			}
		}
	}

	fun add(painter: Painter, delay: Long) {
		internalTimers.add(Timer(painter, requestQueue, this, delay))
	}
}