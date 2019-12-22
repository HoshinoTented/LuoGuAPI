package org.hoshino9.luogu.page

import com.google.gson.JsonObject

abstract class BaseLuoGuPage : LuoGuPage {
	protected lateinit var _feInjection: JsonObject
	override val feInjection: JsonObject
		get() {
			return synchronized(this) {
				if (::_feInjection.isInitialized.not()) refresh()

				_feInjection
			}
		}
}