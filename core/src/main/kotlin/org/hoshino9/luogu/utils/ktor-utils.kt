package org.hoshino9.luogu.utils

import com.google.gson.JsonObject
import io.ktor.client.HttpClient
import io.ktor.client.HttpClientConfig
import io.ktor.client.features.cookies.AcceptAllCookiesStorage
import io.ktor.client.features.cookies.HttpCookies
import io.ktor.client.features.json.GsonSerializer
import io.ktor.client.features.json.JsonFeature
import io.ktor.client.features.websocket.WebSockets
import io.ktor.client.request.HttpRequestBuilder
import io.ktor.client.request.request
import io.ktor.client.response.HttpResponse
import io.ktor.content.TextContent
import io.ktor.http.*
import io.ktor.util.toByteArray
import kotlinx.coroutines.runBlocking
import org.hoshino9.luogu.IllegalStatusCodeException
import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.baseUrl

val JsonObject.asParams: TextContent
	get() {
		return TextContent(toString(), ContentType.Application.Json)
	}

fun HttpClientConfig<*>.emptyClientConfig() {
	install(WebSockets)
}

fun HttpClientConfig<*>.defaultClientConfig(cookiesConfig: HttpCookies.Config.() -> Unit) {
	emptyClientConfig()

	install(HttpCookies) {
		cookiesConfig()
	}

	install(JsonFeature) {
		serializer = GsonSerializer()
	}
}

val emptyClient = HttpClient { emptyClientConfig() }
val defaultClient
	get() = HttpClient {
		defaultClientConfig {
			storage = AcceptAllCookiesStorage()
		}
	}

fun specifiedCookieClient(cookies: List<Pair<Url, Cookie>>): HttpClient {
	return HttpClient {
		defaultClientConfig {
			storage = AcceptAllCookiesStorage().apply {
				cookies.forEach { (url, cookie) ->
					runBlocking {
						addCookie(url, cookie)
					}
				}
			}
		}
	}
}

suspend inline fun HttpClient.apiGet(url: String, block: HttpRequestBuilder.() -> Unit = {}): HttpResponse {
	return request(url) {
		headers.append("x-luogu-type", "content-only")
		block()
	}
}

suspend inline fun LuoGu.apiPost(url: String, block: HttpRequestBuilder.() -> Unit = {}): HttpResponse {
	return client.request("$baseUrl/$url") {
		method = HttpMethod.Post
		headers.append("x-csrf-token", csrfToken())
		block()
	}
}

fun HttpRequestBuilder.referer(ref: String) {
	headers.append("referer", "$baseUrl/$ref")
}

suspend fun HttpResponse.byteData(): ByteArray {
	return content.toByteArray()
}

suspend fun HttpResponse.strData(): String {
	return String(byteData())
}

val ByteArray.asString: String get() = String(this)