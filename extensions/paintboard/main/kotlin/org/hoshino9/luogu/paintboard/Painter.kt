package org.hoshino9.luogu.paintboard

import com.google.gson.JsonObject
import io.ktor.client.call.receive
import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.utils.apiPost
import org.hoshino9.luogu.utils.asParams
import org.hoshino9.luogu.utils.json
import org.hoshino9.luogu.utils.referer

/**
 * 绘画者，最基础的单位
 *
 * @param clientId
 * @param uid
 */
data class Painter(val clientId: String, val uid: Int) {
	private val client by lazy { LuoGu(clientId, uid) }

	suspend fun paint(pos: Pos, color: Int): String {
		val params = JsonObject().apply {
			addProperty("x", pos.x)
			addProperty("y", pos.y)
			addProperty("color", color)
		}

		return client.apiPost("paintBoard/paint") {
			referer("paintBoard")
			body = params.asParams
		}.receive<String>().also {
			json(it).let {
				if (it["status"]?.asInt != 200) {
					throw IllegalStateException(it["data"]?.asString)
				}
			}
		}
	}
}