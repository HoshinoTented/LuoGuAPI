package org.hoshino9.luogu.utils

import com.google.gson.JsonObject
import io.ktor.client.HttpClient
import io.ktor.client.HttpClientConfig
import io.ktor.client.call.HttpClientCall
import io.ktor.client.call.call
import io.ktor.client.engine.okhttp.OkHttp
import io.ktor.client.features.cookies.AcceptAllCookiesStorage
import io.ktor.client.features.cookies.HttpCookies
import io.ktor.client.features.json.GsonSerializer
import io.ktor.client.features.json.JsonFeature
import io.ktor.client.features.websocket.WebSockets
import io.ktor.client.request.HttpRequestBuilder
import io.ktor.content.TextContent
import io.ktor.http.ContentType
import io.ktor.http.Cookie
import io.ktor.http.HttpMethod
import io.ktor.http.Url
import kotlinx.coroutines.runBlocking
import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.LuoGuUtils.baseUrl
import java.net.URLEncoder

private fun String.encode(): String {
	return URLEncoder.encode(this, "UTF-8")
}

val JsonObject.params: TextContent
	get() {
		return TextContent(toString(), ContentType.Application.Json)
	}

val Iterable<Pair<String, String>>.params: TextContent
	get() {
		return TextContent(joinToString(separator = "&") { "${it.first.encode()}=${it.second.encode()}" }, ContentType.Application.FormUrlEncoded)
	}

fun HttpClientConfig<*>.emptyClientConfig() {

}

fun HttpClientConfig<*>.defaultClientConfig(cookiesConfig: HttpCookies.Config.() -> Unit) {
	emptyClientConfig()

	install(HttpCookies) {
		cookiesConfig()
	}

	install(JsonFeature) {
		serializer = GsonSerializer()
	}

	install(WebSockets)
}

val emptyClient = HttpClient { emptyClientConfig() }
val defaultClient
	get() = HttpClient(OkHttp) {
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

suspend fun HttpClient.apiGet(url: String, block: HttpRequestBuilder.() -> Unit = {}): HttpClientCall {
	return call(url) {
		headers.append("x-luogu-type", "content-only")
		block()
	}
}

suspend fun LuoGu.apiPost(url: String, block: HttpRequestBuilder.() -> Unit = {}): HttpClientCall {
	return client.call("$baseUrl/$url") {
		method = HttpMethod.Post
		headers.append("x-csrf-token", csrfToken)
		block()
	}
}

fun HttpRequestBuilder.referer(ref: String) {
	headers.append("referer", "$baseUrl/$ref")
}