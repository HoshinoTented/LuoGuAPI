package org.hoshino9.luogu.extensions.paintboard

import kotlinx.coroutines.*
import org.hoshino9.luogu.LuoGu

@Suppress("MemberVisibilityCanBePrivate", "CanBeParameter")
class PaintBoard(
		clients : List<PaintUser>,
		val targetBoardColor : (Int, Int) -> Int
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
					DrawStatus.SUCCESSFUL -> break@loop
					DrawStatus.FAILED -> {
						if (status.second != """{"data":"操作过于频繁","status":500}""") removeUser(status.second) else {
							println("Failed, try again...(${status.second})")
						}
					}

					DrawStatus.UNKNOWN -> removeUser(status.second)
				}
			}

			println("User ${users[it].user} drew ${x to y} with color $color")

			++ it
			if (it == users.size) it = 0
		}
	}

}