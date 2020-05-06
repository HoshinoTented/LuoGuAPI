package org.hoshino9.luogu

import java.io.FileOutputStream
import java.util.Properties

import org.hoshino9.luogu.test.TestBase

import scala.io.StdIn
import scala.reflect.io.File
import scala.util.Using

object login extends TestBase {
	def main(args: Array[String]): Unit = {
		val verifyCode = client.verifyCode()

		val file = File("verify.png")
		Using(file.outputStream()) { out =>
			out.write(verifyCode)
		}

		println("Please input: <account> <password> <verifyCode>")

		val input = StdIn.readLine()
		val Array(account, password, vc) = input.split(" ")
		client.login(account, password, vc)

		println(client.uid)

		val Some(clientId) = client.clientId
		val Some(uid) = client.uid

		config.setProperty("__client_id", clientId)
		config.setProperty("_uid", uid)
		saveConfig()
	}
}
