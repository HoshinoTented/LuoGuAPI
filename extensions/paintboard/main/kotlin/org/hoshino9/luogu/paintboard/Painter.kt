package org.hoshino9.luogu.paintboard

import org.hoshino9.luogu.LuoGu

data class Painter(val clientId: String, val uid: Int) {
	private val client by lazy { LuoGu(clientId, uid) }

	fun paint(pos: Pos, color: Color) {
		// TODO
	}
}