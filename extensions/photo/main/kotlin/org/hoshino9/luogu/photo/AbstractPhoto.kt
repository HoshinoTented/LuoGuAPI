package org.hoshino9.luogu.photo

import org.hoshino9.luogu.*

abstract class AbstractPhoto : Photo {
	private val urlRegex = Regex("""https://cdn.luogu.org/upload/pic/(\d+)\.(jpg|png|gif)""")

	override val id: String by lazy {
		urlRegex.matchEntire(url)?.run {
			groupValues[1]
		} ?: throw MatchException(urlRegex, url)
	}

	override fun equals(other: Any?): Boolean {
		if (this === other) return true
		if (javaClass != other?.javaClass) return false

		other as AbstractPhoto

		return other.id == id
	}

	override fun hashCode(): Int {
		return id.hashCode()
	}

	override fun toString(): String {
		return id
	}
}