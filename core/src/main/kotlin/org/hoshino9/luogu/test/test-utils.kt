package org.hoshino9.luogu.test

import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.utils.HttpClient
import kotlin.reflect.KVisibility
import kotlin.reflect.full.memberProperties

fun Any.printAllMember() {
	information.run(::println)
}

val Any.information: String
	get() {
		return when (this) {
			is Byte, is Short, is Int, is Long, is Float, is Double, is Boolean, is Unit, is Char, is String -> toString()
			is Collection<*> -> this.joinToString(prefix = "[", postfix = "]") { it?.information.toString() }
			is HttpClient, is LuoGu -> toString()
			else -> {
				this::class.memberProperties.filter { it.visibility == KVisibility.PUBLIC }.joinToString(prefix = "{", postfix = "}") {
					"${it.name} = ${it.call(this)?.information}"
				}
			}
		}
	}