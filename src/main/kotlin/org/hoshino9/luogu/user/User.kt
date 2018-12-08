@file:Suppress("unused")

package org.hoshino9.luogu.user

import org.hoshino9.luogu.LuoGuUtils.baseUrl
import org.hoshino9.luogu.utils.assert
import org.hoshino9.luogu.utils.data
import org.hoshino9.luogu.utils.defaultClient
import org.hoshino9.luogu.utils.getExecute
import org.jsoup.Jsoup
import org.jsoup.nodes.Document

open class User(val uid : String) {
	private val page : Document by lazy {
		defaultClient.getExecute("$baseUrl/space/show?uid=$uid") { resp ->
			resp.assert()
			Jsoup.parse(resp.data !!)
		}
	}

	open val spacePage : UserSpacePage by lazy { UserSpacePage(this) }

	override fun equals(other : Any?) : Boolean {
		if (this === other) return true
		if (other !is User) return false

		if (uid != other.uid) return false

		return true
	}

	override fun hashCode() : Int {
		return uid.hashCode()
	}

	override fun toString() : String {
		return uid
	}
}
