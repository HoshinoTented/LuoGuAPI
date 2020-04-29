package org.hoshino9

import okhttp3.OkHttpClient

package object luogu {
  type HttpClient = OkHttpClient

  val host = "www.luogu.com.cn"
  val baseUrl = s"https://$host"
}
