@file:Suppress("MemberVisibilityCanBePrivate")

package org.hoshino9.okhttp

import okhttp3.Cookie
import okhttp3.CookieJar
import okhttp3.HttpUrl
import okhttp3.HttpUrl.Companion.toHttpUrl
import java.net.CookieManager
import java.net.HttpCookie

open class HoshinoCookieJar : CookieJar {
	val cookieManager = CookieManager()

	override fun saveFromResponse(url: HttpUrl, cookies: List<Cookie>) {
		cookies.forEach {
			cookieManager.cookieStore.add(
					url.toUri(),
					HttpCookie(it.name, it.value).apply {
						domain = (if ("https://${it.domain}".toHttpUrl().topPrivateDomain() == it.domain) "www." else "") + it.domain
					}
			)
		}
	}

	override fun loadForRequest(url : HttpUrl) : List<Cookie> {
		return cookieManager.cookieStore.get(url.toUri()).map {
			Cookie.Builder()
					.domain(it.domain)
					.name(it.name)
					.value(it.value)
					.build()
		}
	}
}