package org.hoshino9.luogu.page

import com.google.gson.JsonObject
import kotlinx.atomicfu.AtomicRef
import kotlinx.atomicfu.atomic

abstract class BaseLuoGuPage : LuoGuPage {
	protected val _feInjection: AtomicRef<JsonObject?> = atomic(null)
	override val feInjection: JsonObject
		@get:Synchronized get() {
			if (_feInjection.value == null) {
				refresh()
			}

			return _feInjection.value !!
		}
}