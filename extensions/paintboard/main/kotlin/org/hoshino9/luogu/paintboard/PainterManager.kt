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

class PainterManager(val photo: PaintBoard, val begin: Pos, override val coroutineContext: CoroutineContext = EmptyCoroutineContext, val globalBoard: () -> PaintBoard) : CoroutineScope {
	private val internalTimer: MutableList<Timer> = LinkedList()
	private val requestQueue: Queue<Timer> = LinkedList()
	private var offset: Pos = Pos(0, 0)

	val currentPos: Pos
		get() {
			return Pos(begin.x + offset.x, begin.y + offset.y)
		}

	val currentColor: Color?
		get() {
			return photo.board[offset]
		}

	val timer: List<Timer> get() = internalTimer

	fun next() {
		offset = Pos(offset.x + 1, offset.y)

		if (photo.board.height == offset.x) {
			offset = Pos(0, offset.y + 1)
		}

		if (photo.board.weight == offset.y) {
			offset = Pos(0, 0)
		}
	}

	fun paint(): Job {
		return launch {
			while (isActive) {
				if (requestQueue.isNotEmpty()) {
					if (offset == Pos(0, 0)) println("New round.")

					val cur = currentColor

					if (cur == null) {
						println("Skip empty color: $currentPos(offset: $offset).")
						next()
						continue
					}

					if (globalBoard().board[currentPos] == cur) {
						println("Skip same color: $currentPos(offset: $offset).")
						next()
						continue
					}

					val front = requestQueue.remove()

					try {
						println("${front.painter.uid} is painting: $currentPos(offset: $offset) with color: $cur")
						front.painter.paint(currentPos, cur)
						next()
						println("${front.painter.uid} is painted.")
					} catch (e: Exception) {
						e.printStackTrace()
					}

					front.resetTimer()
				}
			}
		}
	}

	fun add(painter: Painter, delay: Long) {
		internalTimer.add(Timer(painter, requestQueue, delay))
	}
}