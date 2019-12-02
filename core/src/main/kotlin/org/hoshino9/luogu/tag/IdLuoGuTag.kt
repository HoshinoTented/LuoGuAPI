package org.hoshino9.luogu.tag

open class IdLuoGuTag(override val id: Int) : LuoGuTag {
	override fun toString(): String {
		return id.toString()
	}
}