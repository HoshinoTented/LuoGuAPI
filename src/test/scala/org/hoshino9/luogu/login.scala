package org.hoshino9.luogu

import java.io.{File, FileOutputStream}

import scala.io.StdIn
import scala.util.Using

object login {
  def main(args: Array[String]): Unit = {
    val client = LuoGuClient()
    val verifyCode = client.verifyCode()

    val file = new File("verifyCode.png")
    Using(new FileOutputStream(file)) { out =>
      out.write(verifyCode)
    }

    println("Please input: <account> <password> <verifyCode>")

    val input = StdIn.readLine()
    val Array(account, password, vc) = input.split(" ")
    client.login(account, password, vc)

    println(client.uid)
  }
}
