@file:Suppress("unused")

package org.hoshino9.luogu.user

import org.hoshino9.luogu.LuoGuUtils
import org.hoshino9.luogu.LuoGuUtils.baseUrl
import org.hoshino9.luogu.utils.*
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.jsoup.nodes.Element

open class User(val uid : String, val client : HttpClient = defaultClient) {
	companion object {
		@JvmName("newInstance")
		operator fun invoke(elem : Element) : User {
			if (elem.children().size != 1) return HasBadgeUser(elem)

			return elem.child(0).attr("href").run(LuoGuUtils::userFromUrl)
		}
	}

	private val page : Document = run {
		client.executeGet("$baseUrl/space/show?uid=$uid") { resp ->
			resp.assert()
			Jsoup.parse(resp.strData)
		}
	}

	open val spacePage : UserSpacePage = run { UserSpacePage(this) }

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
