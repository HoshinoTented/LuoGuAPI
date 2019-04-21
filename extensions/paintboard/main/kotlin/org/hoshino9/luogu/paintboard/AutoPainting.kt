package org.hoshino9.luogu.paintboard

import kotlinx.coroutines.*

@Suppress("MemberVisibilityCanBePrivate", "CanBeParameter")
class AutoPainting(
		clients : List<PaintUser>,
		val targetBoardColor : (Int, Int) -> Int = { x, y -> DefaultLuoGu.boardMatrix[x][y].toString().toInt(32) }
) {
	private var it = 0

	val users = clients.toMutableList()
	val currentUser get() = users[it]

	private fun removeUser(msg : String) {
		println("Failed, removed user: ${users[it].user}($msg)")

		users.removeAt(it)
		if (it == users.size) it = 0
	}

	private fun nextUser() : PaintUser {
		++ it
		if (it == users.size) it = 0

		return currentUser
	}

	/**
	 * 绘画
	 * @param x 色块的 x 坐标
	 * @param y 色块的 y 坐标
	 * @param color 颜色在 [colorList] 中的索引
	 */
	fun draw(x : Int, y : Int, color : Int) = runBlocking {
		fun checkColor(x : Int, y : Int) : Boolean {
			return if (targetBoardColor(x, y) == color) {
				println("Skipped ($x, $y)")

				true
			} else false
		}

		if (checkColor(x, y).not()) {
			println("Try to draw ($x, $y)...")

			loop@ while (true) {
				println("Waiting...")
				currentUser.timer.await()

				if (checkColor(x, y)) break@loop

				val status = currentUser.draw(x, y, color)
				when (status.first) {
					DrawStatus.SUCCESSFUL -> {
						println("Successful! User $currentUser drew ${x to y} with color $color(${status.second})")
						nextUser()

						break@loop
					}

					DrawStatus.FAILED -> {
						if (status.second != "操作过于频繁") removeUser(status.second) else {
							println("Failed, turn to next user: ${nextUser()} (${status.second})")
						}
					}

					DrawStatus.NO_LOGIN, DrawStatus.UNKNOWN -> removeUser(status.second)
				}
			}
		}
	}

	fun draw(beginX : Int, beginY : Int, pos : ColorPos) {
		draw(pos.x + beginX, pos.y + beginY, pos.color)
	}
}