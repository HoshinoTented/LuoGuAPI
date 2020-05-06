package org.hoshino9

import okhttp3.OkHttpClient

package object luogu {
	type JavaList[T] = java.util.List[T]
	type HttpClient = OkHttpClient

	val host = "www.luogu.com.cn"
	val baseUrl = s"https://$host"
}
