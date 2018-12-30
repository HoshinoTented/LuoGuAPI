@file:Suppress("unused")

package org.hoshino9.luogu.utils

import okhttp3.*
import org.hoshino9.luogu.IllegalStatusCodeException
import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.LuoGuUtils
import org.hoshino9.okhttp.HoshinoCookieJar

// Params
fun <K, V> Map<K, V>.params() : RequestBody {
	return FormBody.Builder().apply {
		forEach { k, v ->
			add(k.toString(), v.toString())
		}
	}.build()
}

// Headers
fun referer(url : String = "") : Headers = Headers.Builder().add("referer", "${LuoGuUtils.baseUrl}/$url").build()

// Request
fun LuoGu.postRequest(url : String, body : RequestBody, headers : Headers) : Request = Request.Builder()
		.url("${LuoGuUtils.baseUrl}/$url")
		.post(body)
		.headers(headers)
		.addHeader("x-csrf-token", csrfToken)
		.build()

@JvmOverloads
fun getRequest(url : String = "") : Request = Request.Builder()
		.url(url)
		.addHeader("User-Agent", USER_AGENT)
		.build()

@Deprecated("Request without referer will be refused", ReplaceWith("executePost"))
inline fun <T> LuoGu.executePost(url : String = "", body : RequestBody = emptyMap<String, String>().params(), action : (Response) -> T) : T {
	return executePost(url, body, Headers.Builder().build(), action)
}

inline fun <T> LuoGu.executePost(url : String = "", body : RequestBody = emptyMap<String, String>().params(), headers : Headers, action : (Response) -> T) : T {
	return client.newCall(postRequest(url, body, headers)).execute().let { resp ->
		resp.run(action).apply {
			resp.close()
		}
	}
}

inline fun <T> LuoGu.executeGet(url : String = "", action : (Response) -> T) : T = client.executeGet("${LuoGuUtils.baseUrl}/$url", action)
inline fun <T> HttpClient.executeGet(url : String = "", action : (Response) -> T) : T {
	return newCall(getRequest(url)).execute().let { resp ->
		resp.run(action).apply {
			resp.close()
		}
	}
}

// Client
val emptyClient : HttpClient get() = OkHttpClient()
val defaultClient : HttpClient
	get() = OkHttpClient.Builder()
			.cookieJar(HoshinoCookieJar())
			.build()

// Response
fun Response.assert() {
	if (! isSuccessful) throw IllegalStatusCodeException(this)
}

val Response.data : String? get() = this.body()?.string()