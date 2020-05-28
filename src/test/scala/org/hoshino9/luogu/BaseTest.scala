package org.hoshino9.luogu

import org.junit.Test
import play.api.libs.json.Json

class BaseTest {
  @Test
  def testGetting(): Unit = {
    val client = LuoGuClient()
    println(client.get("https://www.luogu.com.cn/api/verify/captcha"))
    println(client.clientId)
    println(client.currentData)
  }

  @Test
  def testPosting(): Unit = {
    val client = LuoGuClient()
	  println(client.post("https://httpbin.org/post", Json.obj()).body().string())
  }
}