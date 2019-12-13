@file:Suppress("unused")

package org.hoshino9.luogu.utils

import okhttp3.OkHttpClient

typealias HttpClient = OkHttpClient

const val SEPARATOR = "&"
const val EQUAL = "="
const val USER_AGENT = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36"

val percentRegex = Regex("(.+?) / ?(.+?)")