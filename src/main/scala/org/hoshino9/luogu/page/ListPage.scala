package org.hoshino9.luogu.page

trait ListPage {
	val count: Int
	val perPage: Int

	def pageCount: Int = {
		Math.ceil(count.toDouble / perPage).toInt
	}
}
