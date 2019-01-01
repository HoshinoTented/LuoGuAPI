package org.hoshino9.luogu.extensions.paintboard

import kotlinx.coroutines.*
import org.hoshino9.luogu.LuoGu

@Suppress("MemberVisibilityCanBePrivate", "CanBeParameter")
class AutoPainting(
		clients : List<PaintUser>,
		val targetBoardColor : (Int, Int) -> Int = { x, y -> DefaultLuoGu.boardMatrix[x][y].toString().toInt(32) }
) {
	private var it = 0

	val users = clients.toMutableList()
	val currentUser get() = users[it]
	val currentClient get() = currentUser.user.luogu.client

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

				val status = users[it].draw(x, y, color)
				when (status.first) {
					DrawStatus.SUCCESSFUL -> {
						println("Successful! User ${users[it]} drew ${x to y} with color $color(${status.second})")
						break@loop
					}

					DrawStatus.FAILED -> {
						if (status.second != "操作过于频繁") removeUser(status.second) else {
							println("Failed, turn to next user: ${nextUser()} (${status.second})")
						}
					}

					DrawStatus.UNKNOWN -> removeUser(status.second)
				}
			}

			nextUser()
		}
	}

}