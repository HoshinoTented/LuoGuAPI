package org.hoshino9.luogu

import org.apache.http.HttpEntity
import org.apache.http.client.HttpClient
import org.apache.http.client.entity.UrlEncodedFormEntity
import org.apache.http.client.methods.HttpPost
import org.apache.http.impl.client.DefaultHttpClient
import org.apache.http.impl.client.HttpClients
import org.apache.http.message.BasicNameValuePair
import org.apache.http.util.EntityUtils
import java.nio.charset.Charset

const val SEPARATOR = "&"
const val EQUAL = "="

/**
 * 把 Map 对象转化为 HttpEntity 对象
 */
fun <K, V> Map<K, V>.stringEntity() : HttpEntity {
	return UrlEncodedFormEntity(
			map { (k, v) ->
				BasicNameValuePair(k.toString(), v.toString())
			}, Charset.forName("UTF-8")
	)
}

val HttpEntity.data : String get() = EntityUtils.toString(this)

fun LuoGu.postRequest(url : String) = HttpPost("${LuoGu.baseUrl}/$url").apply { addHeader("x-csrf-token", this@postRequest.csrfToken) }

fun <T : CharSequence> Iterable<T>.firstNotBlackOrNull() : T? = firstOrNull { it.isNotBlank() }

val defaultClient : HttpClient get() = HttpClients.createDefault()