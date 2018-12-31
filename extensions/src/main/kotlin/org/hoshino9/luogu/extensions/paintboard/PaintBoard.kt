package org.hoshino9.luogu.extensions.paintboard

import kotlinx.coroutines.*
import org.hoshino9.luogu.LuoGu

@Suppress("MemberVisibilityCanBePrivate", "CanBeParameter")
class PaintBoard(
		clients : List<LuoGu>,
		val timeLimit : (List<LuoGu>) -> Long = { 30 * 1000 },
		val targetBoardColor : (Int, Int) -> Int,
		val coroutineScope : CoroutineScope = GlobalScope
) {
	private var it = 0
	private var timer = coroutineScope.async { Unit }

	val clients = clients.toMutableList()
	val currentClient get() = clients[it].client

	private fun removeUser(msg : String) {
		println("Failed, removed user: ${clients[it].loggedUser}($msg)")

		clients.removeAt(it)
		if (it == clients.size) it = 0
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
				timer.await()

				if (checkColor(x, y)) break@loop

				val status = clients[it].draw(x, y, color)
				when (status.first) {
					DrawStatus.SUCCESSFUL -> {
						timer = coroutineScope.async { delay(timeLimit(clients)) }
						break@loop
					}

					DrawStatus.FAILED -> {
						if (status.second != """{"data":"操作过于频繁","status":500}""") removeUser(status.second) else {
							println("Failed, try again...(${status.second})")
							timer = coroutineScope.async { delay(timeLimit(clients)) }
						}
					}

					DrawStatus.UNKNOWN -> removeUser(status.second)
				}
			}

			println("User ${clients[it].loggedUser} drew ${x to y} with color $color")

			++ it
			if (it == clients.size) it = 0
		}
	}

}