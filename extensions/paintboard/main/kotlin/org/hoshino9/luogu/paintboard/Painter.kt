package org.hoshino9.luogu.paintboard

import com.google.gson.JsonObject
import io.ktor.client.HttpClient
import io.ktor.client.call.receive
import io.ktor.client.features.ClientRequestException
import io.ktor.client.request.post
import io.ktor.client.request.request
import io.ktor.client.response.HttpResponse
import io.ktor.http.HttpMethod
import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.LuoGuUtils.baseUrl
import org.hoshino9.luogu.utils.*

/**
 * 绘画者，最基础的单位
 */
data class Painter(val client: HttpClient, val id: Int, val url: String = "$baseUrl/paintBoard/paint") {
	suspend fun paint(pos: Pos, color: Int): String {
		val params = JsonObject().apply {
			addProperty("x", pos.x)
			addProperty("y", pos.y)
			addProperty("color", color)
		}

		try {
			return client.request<HttpResponse>(url) {
				method = HttpMethod.Post
				referer("paintBoard")
				body = params.asParams
			}.receive()
		} catch (e: ClientRequestException) {
			throw IllegalStateException(json(e.response.strData())["data"]?.asString)
		}
	}
}