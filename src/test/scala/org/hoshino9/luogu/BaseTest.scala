package org.hoshino9.luogu

import com.google.gson.JsonObject
import org.junit.Test

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
    println(client.post("https://httpbin.org/post", new JsonObject()).body().string())
  }
}