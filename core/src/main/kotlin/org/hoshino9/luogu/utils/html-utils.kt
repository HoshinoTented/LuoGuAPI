package org.hoshino9.luogu.utils

import io.ktor.client.request.get
import kotlinx.coroutines.runBlocking
import okhttp3.OkHttpClient
import org.jsoup.Jsoup
import org.jsoup.nodes.Document

//fun HttpClient.page(url : String) : Document {
//	return runBlocking {
//		get<String>(url).let {
//			Jsoup.parse(it)
//		}
//	}
//}