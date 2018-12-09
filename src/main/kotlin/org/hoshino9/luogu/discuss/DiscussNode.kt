package org.hoshino9.luogu.discuss

import org.hoshino9.luogu.user.User
import org.hoshino9.luogu.utils.HasUrl
import org.hoshino9.luogu.utils.HttpClient
import org.hoshino9.luogu.utils.defaultClient
import org.jsoup.nodes.Element

interface DiscussNode : HasUrl {
	companion object {
		@JvmName("newInstance")
		operator fun invoke(elem : Element, client : HttpClient = defaultClient) : DiscussNode {
			return DefaultDiscussNode(elem, client)
		}
	}

	val isTopping : Boolean
	val id : String
	val replyCount : Int
	val user : User
	val forum : DiscussListPage
	val title : String
	val postDate : String
	val lastCommentUser : User?
	val lastCommentDate : String
	val infoPage : DiscussInfoPage
}