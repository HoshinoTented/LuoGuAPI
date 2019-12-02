@file:Suppress("unused")

package org.hoshino9.luogu.utils

import com.google.gson.JsonObject
import okhttp3.*
import okhttp3.MediaType.Companion.toMediaTypeOrNull
import okhttp3.RequestBody.Companion.toRequestBody
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
	return toString().toRequestBody("application/json".toMediaTypeOrNull())
}

// Headers
fun referer(url: String = ""): Headers = Headers.Builder().add("referer", "${LuoGuUtils.baseUrl}/$url").build()

val emptyHeaders: Headers
	get() {
		return Headers.Builder().build()
	}

// Request
fun HttpClient.postRequest(url: String, body: RequestBody, headers: Headers): Request = Request.Builder()
		.url(url)
		.post(body)
		.headers(headers)
		.build()

@JvmOverloads
fun getRequest(url: String = "", headers: Headers): Request = Request.Builder()
		.url(url)
		.headers(headers)
		.addHeader("User-Agent", USER_AGENT)
		.build()

inline fun <T> LuoGu.executePost(url: String = "", body: RequestBody = emptyParams(), headers: Headers /* 一般是 referer */, action: (Response) -> T): T =
		client.executePost("${LuoGuUtils.baseUrl}/$url", body, headers.newBuilder().add("x-csrf-token", csrfToken).build(), action)

inline fun <T> HttpClient.executePost(url: String = "", body: RequestBody = emptyParams(), headers: Headers = emptyHeaders, action: (Response) -> T): T {
	return newCall(postRequest(url, body, headers)).execute().use(action)
}

inline fun <T> LuoGu.executeGet(url: String = "", headers: Headers = emptyHeaders, action: (Response) -> T): T = client.executeGet("${LuoGuUtils.baseUrl}/$url", headers, action)
inline fun <T> HttpClient.executeGet(url: String = "", headers: Headers = emptyHeaders, action: (Response) -> T): T {
	return newCall(getRequest(url, headers)).execute().use { resp ->
		resp.run(action)
	}
}

inline fun <T> HttpClient.contentOnlyGet(url: String, action: (Response) -> T): T {
	return executeGet(url, Headers.headersOf("x-luogu-type", "content-only"), action)        //或者在 url 后添加 _contentOnly=1 但个人不推荐
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
	if (! isSuccessful) throw IllegalStatusCodeException(code.toString(), strData)
}

fun Response.assertJson() {
	if (! isSuccessful) json(strData).delegate.let {
		val status: Int? by it

		if (status != null) {
			val data: String by it

			throw IllegalStatusCodeException(status, data)
		}
	}
}

val Response.strData: String
	get() {
		return this.body !!.string().apply {
			// TODO LOG
		}
	}

val Response.dataStream: InputStream
	get() {
		return this.body !!.byteStream()
	}