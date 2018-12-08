@file:Suppress("unused")

package org.hoshino9.luogu

import com.google.gson.Gson
import com.google.gson.GsonBuilder
import okhttp3.*
import org.hoshino9.luogu.LuoGuUtils.baseUrl
import org.hoshino9.luogu.record.*
import org.hoshino9.luogu.record.status.RecordStatus
import org.hoshino9.luogu.record.status.RecordStatusAdapter
import org.hoshino9.luogu.record.status.RecordStatusStatusAdapter
import org.hoshino9.okhttp.HoshinoCookieJar
import org.json.JSONObject
import okhttp3.Callback as OkHttpCallback

const val SEPARATOR = "&"
const val EQUAL = "="
const val USER_AGENT = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36"

// Utils
val globalGson : Gson by lazy {
	GsonBuilder()
			.registerTypeAdapter(TestCase.Status::class.java, TestCaseStatusAdapter)
			.registerTypeAdapter(RecordStatus.Status::class.java, RecordStatusStatusAdapter)
			.registerTypeAdapter(RecordStatus.Detail::class.java, RecordStatus.Detail.Adapter)
			.registerTypeAdapter(RecordStatus::class.java, RecordStatusAdapter)
			.create()
}

// Params
fun <K, V> Map<K, V>.params() : RequestBody {
	return FormBody.Builder().apply {
		forEach { k, v ->
			add(k.toString(), v.toString())
		}
	}.build()
}

// Headers
fun referer(url : String = "") : Headers = Headers.Builder().add("referer", "$baseUrl/$url").build()

// Request
fun LuoGu.postRequest(url : String, body : RequestBody, headers : Headers) : Request = Request.Builder()
		.url("$baseUrl/$url")
		.post(body)
		.headers(headers)
		.addHeader("x-csrf-token", csrfToken)
		.build()

@JvmOverloads
fun getRequest(url : String = "") : Request = Request.Builder()
		.url(url)
		.addHeader("User-Agent", USER_AGENT)
		.build()

@Deprecated("Request without referer will be refused", ReplaceWith("postExecute"))
inline fun <T> LuoGu.postExecute(url : String = "", body : RequestBody = emptyMap<String, String>().params(), action : (Response) -> T) : T {
	return postExecute(url, body, Headers.Builder().build(), action)
}

inline fun <T> LuoGu.postExecute(url : String = "", body : RequestBody = emptyMap<String, String>().params(), headers : Headers, action : (Response) -> T) : T {
	return client.newCall(postRequest(url, body, headers)).execute().let { resp ->
		resp.run(action).apply {
			resp.close()
		}
	}
}

inline fun <T> LuoGu.getExecute(url : String = "", action : (Response) -> T) : T = client.getExecute("$baseUrl/$url", action)
inline fun <T> OkHttpClient.getExecute(url : String = "", action : (Response) -> T) : T {
	return newCall(getRequest(url)).execute().let { resp ->
		resp.run(action).apply {
			resp.close()
		}
	}
}

// Client
val emptyClient : OkHttpClient get() = OkHttpClient()
val defaultClient : OkHttpClient
	get() = OkHttpClient.Builder()
			.cookieJar(HoshinoCookieJar())
			.build()

// Response
fun Response.assert() {
	if (! isSuccessful) throw IllegalStatusCodeException(this)
}

val Response.data : String? get() = this.body()?.string()

// Json
inline fun <T> json(content : String, init : JSONObject.() -> T) : T {
	return JSONObject(content).run(init)
}

fun json(content : String) : JSONObject {
	return JSONObject(content)
}

// Utils
inline fun <T> Iterable<T>.splitWith(block : (T) -> Boolean) : List<List<T>> {
	val result = ArrayList<List<T>>()
	var tmpList = ArrayList<T>()

	forEach { elem ->

		if (block(elem) && tmpList.isNotEmpty()) {
			result.add(tmpList)
			tmpList = ArrayList()
		}

		tmpList.add(elem)
	}

	if (tmpList.isNotEmpty()) result.add(tmpList)

	return result
}