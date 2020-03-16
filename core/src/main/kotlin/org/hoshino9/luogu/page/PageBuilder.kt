package org.hoshino9.luogu.page

interface PageBuilder<T> {
	fun build(): T
}