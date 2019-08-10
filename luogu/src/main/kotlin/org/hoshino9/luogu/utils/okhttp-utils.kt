@file:Suppress("unused")

package org.hoshino9.luogu.utils

import com.google.gson.JsonObject
import okhttp3.*
import org.hoshino9.luogu.IllegalStatusCodeException
import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.LuoGuUtils
import org.hoshino9.okhttp.HoshinoCookieJar
import java.io.InputStream

fun emptyParams(): RequestBody {
	return FormBody.Builder().build()
}

// Params
fun <K : Any, V : Any> Iterable<Pair<K, V>>.params(): RequestBody {
	return FormBody.Builder().apply {
		forEach { (k, v) ->
			add(k.toString(), v.toString())
		}
	}.build()
}

fun JsonObject.params(): RequestBody {
	return RequestBody.create(MediaType.parse("application/json"), toString())
}

// Headers
fun referer(url: String = ""): Headers = Headers.Builder().add("referer", "${LuoGuUtils.baseUrl}/$url").build()

val emptyHeaders: Headers
	get() {
		return Headers.Builder().build()
	}

// Request
fun LuoGu.postRequest(url: String, body: RequestBody, headers: Headers): Request = Request.Builder()
		.url("${LuoGuUtils.baseUrl}/$url")
		.post(body)
		.headers(headers)
		.addHeader("x-csrf-token", csrfToken)
		.build()

@JvmOverloads
fun getRequest(url: String = "", headers: Headers): Request = Request.Builder()
		.url(url)
		.headers(headers)
		.addHeader("User-Agent", USER_AGENT)
		.build()

inline fun <T> LuoGu.executePost(url: String = "", body: RequestBody = emptyParams(), headers: Headers /* 一般是 referer */, action: (Response) -> T): T {
	return client.newCall(postRequest(url, body, headers)).execute().use { resp ->
		resp.run(action)
	}
}

inline fun <T> LuoGu.executeGet(url: String = "", headers: Headers = emptyHeaders, action: (Response) -> T): T = client.executeGet("${LuoGuUtils.baseUrl}/$url", headers, action)
inline fun <T> HttpClient.executeGet(url: String = "", headers: Headers = emptyHeaders, action: (Response) -> T): T {
	return newCall(getRequest(url, headers)).execute().use { resp ->
		resp.run(action)
	}
}

inline fun <T> HttpClient.contentOnlyGet(url: String, action: (Response) -> T): T {
	return executeGet(url, Headers.of("x-luogu-type", "content-only"), action)        //或者在 url 后添加 _contentOnly=1 但个人不推荐
}

fun HttpClient.apiGet(url: String): JsonObject {
	return contentOnlyGet(url) {
		it.assert()
		json(it.strData)
	}
}

// Client
val emptyClient: HttpClient /*get() */ = OkHttpClient()
val defaultClient: HttpClient
	get() = OkHttpClient.Builder()
			.cookieJar(HoshinoCookieJar())
			.build()

// Response
fun Response.assert() {
	if (! isSuccessful) throw IllegalStatusCodeException(code().toString(), strData)
}

val Response.strData: String
	get() {
		return this.body() !!.string().apply {
			// TODO LOG
		}
	}

val Response.dataStream: InputStream
	get() {
		return this.body() !!.byteStream()
	}