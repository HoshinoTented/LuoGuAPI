package org.hoshino9.luogu.paintboard

import kotlinx.coroutines.*
import java.util.LinkedList
import java.util.Queue
import kotlin.coroutines.CoroutineContext
import kotlin.coroutines.EmptyCoroutineContext

/**
 * 计时器
 *
 * 每经过 [delay] 毫秒，就会向请求队列 [queue] 中添加计时器
 *
 * @param painter 当前计时器对应的绘画者
 * @param queue 目标请求队列
 * @param scope 协程域
 * @param delay 延时，单位为毫秒
 */
data class Timer(val painter: Painter, private val queue: Queue<Timer>, val scope: CoroutineScope = GlobalScope, val delay: Long) {
	private lateinit var timer: Job

	init {
		resetTimer()
	}

	fun resetTimer() {
		timer = scope.launch {
			delay(this@Timer.delay)

			queue.add(this@Timer)

			println("${painter.uid} is ready.")
		}
	}

	fun canPaint(): Boolean {
		return timer.isCompleted
	}
}

/**
 * 绘画者管理器
 *
 * @param photoProvider 提供绘画的坐标和颜色
 * @param begin 开始绘画的坐标
 * @param coroutineContext 协程上下文
 * @param boardProvider 提供全局绘板
 */
class PainterManager(val photoProvider: PhotoProvider, val begin: Pos, override val coroutineContext: CoroutineContext = EmptyCoroutineContext, val boardProvider: suspend () -> Board) : CoroutineScope {
	private val internalTimers: MutableList<Timer> = LinkedList()
	private val internalRequestQueue: Queue<Timer> = LinkedList()
	private var internalJob: Job? = null

	val job: Job? get() = internalJob
	val timers: List<Timer> get() = internalTimers
	val requestQueue: Collection<Timer> get() = internalRequestQueue

	/**
	 * 开始进行绘画
	 *
	 * 同一时间，同一 PainterManager 只能有同一个绘画线程
	 */
	fun paint() {
		val job = internalJob

		if (job != null && job.isActive) throw IllegalStateException("Job is working.")

		internalJob = launch {
			while (isActive) {
				if (internalRequestQueue.isNotEmpty()) {
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

					val current = internalRequestQueue.remove()

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
		internalTimers.add(Timer(painter, internalRequestQueue, this, delay))
	}
}