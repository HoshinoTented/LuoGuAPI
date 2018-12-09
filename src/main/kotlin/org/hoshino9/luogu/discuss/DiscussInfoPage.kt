package org.hoshino9.luogu.discuss

import org.hoshino9.luogu.utils.HasUrl
import org.hoshino9.luogu.utils.HttpClient
import org.hoshino9.luogu.utils.defaultClient

interface DiscussInfoPage : HasUrl {
	companion object {
		@JvmName("newInstance")
		operator fun invoke(id : String, page : Int = 1, client : HttpClient = defaultClient) : DiscussInfoPage {
			return DefaultDiscussInfoPage(id, page, client)
		}
	}

	val page : Int
	val mainComment : MainComment
	val comments : List<DiscussComment>
	val forum : String
	val id : String
}