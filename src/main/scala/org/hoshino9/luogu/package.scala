package org.hoshino9

import okhttp3.OkHttpClient

package object luogu {
	type HttpClient = OkHttpClient

	val domain = ".luogu.com.cn"
	val host = s"www$domain"
	val baseUrl = s"https://$host"
}
