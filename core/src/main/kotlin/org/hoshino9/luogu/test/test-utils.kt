package org.hoshino9.luogu.test

import kotlin.reflect.full.memberProperties

fun Any.printAllMember() {
	this::class.memberProperties.forEach {
		if (it.isOpen) {
			println("${it.name} = ${it.call(this)}")
		}
	}
}