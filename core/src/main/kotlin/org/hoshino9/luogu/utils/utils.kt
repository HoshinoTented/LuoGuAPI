@file:Suppress("unused")

package org.hoshino9.luogu.utils

import okhttp3.OkHttpClient
import okhttp3.Callback as OkHttpCallback

typealias HttpClient = OkHttpClient

const val SEPARATOR = "&"
const val EQUAL = "="
const val USER_AGENT = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36"

val percentRegex = Regex("(.+?) / ?(.+?)")


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

inline fun <T> Iterator<T>.takeWhile(block : (T) -> Boolean) : List<T> {
	return ArrayList<T>().also { result ->
		while (hasNext()) {
			val next = next()
			if (block(next)) result.add(next) else break
		}
	}
}