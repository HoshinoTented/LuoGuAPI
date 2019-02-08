package org.hoshino9.luogu.utils

import okhttp3.OkHttpClient
import org.jsoup.Jsoup
import org.jsoup.nodes.Document

fun OkHttpClient.page(url : String) : Document {
	return executeGet(url) {
		it.assert()
		Jsoup.parse(it.strData)
	}
}