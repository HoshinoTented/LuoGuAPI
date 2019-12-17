@file:Suppress("unused")

package org.hoshino9.luogu.utils

import com.google.gson.JsonElement
import io.ktor.client.HttpClient
import kotlin.reflect.KClass

typealias HttpClient = HttpClient

const val SEPARATOR = "&"
const val EQUAL = "="
const val USER_AGENT = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36"

abstract class Deserializable<T : Any>(private val `class`: KClass<T>) {
	@JvmName("fromJson")
	operator fun invoke(source: JsonElement): T {
		return gson.fromJson(source, `class`.java)
	}
}