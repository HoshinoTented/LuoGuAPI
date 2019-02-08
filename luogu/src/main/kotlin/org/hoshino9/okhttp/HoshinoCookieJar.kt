@file:Suppress("MemberVisibilityCanBePrivate")

package org.hoshino9.okhttp

import okhttp3.Cookie
import okhttp3.CookieJar
import okhttp3.HttpUrl
import java.net.CookieManager
import java.net.HttpCookie

open class HoshinoCookieJar : CookieJar {
	val cookieManager = CookieManager()

	override fun saveFromResponse(url : HttpUrl, cookies : MutableList<Cookie>) {
		cookies.forEach {
			cookieManager.cookieStore.add(
					url.uri(),
					HttpCookie(it.name(), it.value()).apply {
						domain = (if (HttpUrl.get("https://" + it.domain()).topPrivateDomain() == it.domain()) "www." else "") + it.domain()
					}
			)
		}
	}

	override fun loadForRequest(url : HttpUrl) : List<Cookie> {
		return cookieManager.cookieStore.get(url.uri()).map {
			Cookie.Builder()
					.domain(it.domain)
					.name(it.name)
					.value(it.value)
					.build()
		}
	}
}