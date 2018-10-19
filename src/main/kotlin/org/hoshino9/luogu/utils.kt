package org.hoshino9.luogu

import org.apache.http.HttpEntity
import org.apache.http.client.entity.UrlEncodedFormEntity
import org.apache.http.client.methods.HttpPost
import org.apache.http.message.BasicNameValuePair
import org.apache.http.util.EntityUtils
import java.nio.charset.Charset

/**
 * 把 Map 对象转化为 HttpEntity 对象
 */
fun <K, V> Map<K, V>.entity() : HttpEntity = UrlEncodedFormEntity(
		map { (k, v) ->
			BasicNameValuePair(k.toString(), v.toString())
		}, Charset.forName("UTF-8")
)

val HttpEntity.data : String get() = EntityUtils.toString(this)

fun LuoGu.postRequest(url : String) = HttpPost("${LuoGu.baseUrl}/$url").apply { addHeader("x-csrf-token", this@postRequest.csrfToken) }