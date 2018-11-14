package org.hoshino9.luogu

import okhttp3.*
import org.hoshino9.okhttp.LuoGuOnlyCookieJar
import okhttp3.Callback as OkHttpCallback

const val SEPARATOR = "&"
const val EQUAL = "="
const val USER_AGENT = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36"

fun <K, V> Map<K, V>.params() : RequestBody {
	return FormBody.Builder().apply {
		forEach { k, v ->
			addEncoded(k.toString(), v.toString())
		}
	}.build()
}

fun LuoGu.postRequest(url : String, body : RequestBody = emptyMap<String, String>().params()) : Request = Request.Builder()
		.url("${LuoGu.baseUrl}/$url")
		.addHeader("x-csrf-token", csrfToken)
		.post(body)
		.build()

@JvmOverloads
fun getRequest(url : String = "") : Request = Request.Builder()
		.url(url)
		.addHeader("User-Agent", USER_AGENT)
		.build()

inline fun <T> LuoGu.postExecute(url : String = "", body : RequestBody = emptyMap<String, String>().params(), action : (Response) -> T) : T = client.newCall(postRequest(url, body)).execute().run(action)
inline fun <T> LuoGu.getExecute(url : String = "", action : (Response) -> T) : T = client.getExecute("${LuoGu.baseUrl}/$url", action)
inline fun <T> OkHttpClient.getExecute(url : String = "", action : (Response) -> T) : T = newCall(getRequest(url)).execute().run(action)

fun <T : CharSequence> Iterable<T>.firstNotBlackOrNull() : T? = firstOrNull { it.isNotBlank() }

val emptyClient : OkHttpClient = OkHttpClient()
val defaultClient : OkHttpClient = OkHttpClient.Builder()
		.cookieJar(LuoGuOnlyCookieJar())
		.build()


fun Response.assert() {
	if (! isSuccessful) throw StatusCodeException(this)
}

val Response.data : String? get() = this.body()?.string()