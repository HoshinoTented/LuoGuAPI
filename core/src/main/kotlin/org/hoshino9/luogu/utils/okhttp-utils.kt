package org.hoshino9.luogu.utils

import com.google.gson.JsonObject
import io.ktor.client.HttpClient
import io.ktor.client.HttpClientConfig
import io.ktor.client.call.HttpClientCall
import io.ktor.client.call.call
import io.ktor.client.features.cookies.AcceptAllCookiesStorage
import io.ktor.client.features.cookies.HttpCookies
import io.ktor.client.features.json.GsonSerializer
import io.ktor.client.features.json.JsonFeature
import io.ktor.client.request.HttpRequestBuilder
import io.ktor.content.TextContent
import io.ktor.http.ContentType
import io.ktor.http.Cookie
import io.ktor.http.Url
import kotlinx.coroutines.runBlocking
import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.LuoGuUtils.baseUrl

val JsonObject.params: TextContent
	get() {
		return TextContent(toString(), ContentType.Application.Json)
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

suspend fun HttpClient.apiGet(url: String): HttpClientCall {
	return call(url) {
		headers.append("x-luogu-type", "content-only")
	}
}

suspend fun LuoGu.apiPost(url: String, block: HttpRequestBuilder.() -> Unit): HttpClientCall {
	return client.call(url) {
		headers.append("User-Agent", USER_AGENT)
		headers.append("x-csrf-token", csrfToken)
		block()
	}
}

fun HttpRequestBuilder.referer(ref: String) {
	headers.append("referer", "$baseUrl/ref")
}