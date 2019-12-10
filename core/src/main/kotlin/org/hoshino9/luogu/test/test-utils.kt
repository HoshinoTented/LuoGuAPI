package org.hoshino9.luogu.test

import kotlin.reflect.KVisibility
import kotlin.reflect.full.declaredMemberProperties
import kotlin.reflect.full.memberProperties

fun Any.printAllMember() {
	this::class.memberProperties.forEach {
		if (it.visibility == KVisibility.PUBLIC) {
			println("${it.name} = ${it.call(this)}")
		}
	}
}