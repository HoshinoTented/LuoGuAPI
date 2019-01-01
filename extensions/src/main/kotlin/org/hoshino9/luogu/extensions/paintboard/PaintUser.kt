package org.hoshino9.luogu.extensions.paintboard

import kotlinx.coroutines.*
import org.hoshino9.luogu.user.LoggedUser
import org.hoshino9.luogu.utils.*

@Suppress("MemberVisibilityCanBePrivate")
class PaintUser(val user : LoggedUser, val coroutineScope : CoroutineScope = GlobalScope, var timer : Deferred<Unit> = coroutineScope.async { Unit }) {
	fun draw(x : Int, y : Int, color : Int) : Pair<DrawStatus, String> {
		return user.luogu.executePost("paintBoard/paint",
				listOf(
						"x" to x,
						"y" to y,
						"color" to color
				).params(), referer("paintBoard")) { resp ->
			resp.assert()

			json(resp.data !!) {
				when (this["status"]) {
					200 -> {
						timer = coroutineScope.async { delay(30 * 1000) }

						DrawStatus.SUCCESSFUL
					}

					500 -> {
						timer = coroutineScope.async { delay(10 * 1000) }
						DrawStatus.FAILED
					}

					else -> DrawStatus.UNKNOWN
				} to this.toString()
			}
		}
	}
}