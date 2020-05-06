package org.hoshino9.luogu.test

import java.util.Properties

import org.hoshino9.luogu.LuoGuClient

import scala.reflect.io.File

trait TestBase {
	val configFile: File = File("./user.properties")
	val config: Properties = {
		val properties = new Properties()
		properties.load(configFile.inputStream())
		properties
	}

	val client: LuoGuClient = {
		val clientId = config.getProperty("__client_id")
		val uid = config.getProperty("_uid")

		LuoGuClient(Some(clientId), Some(uid))
	}

	def saveConfig(): Unit = {
		config.store(configFile.outputStream(), null)
	}
}
