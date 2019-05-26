package org.hoshino9.luogu.comment

import org.hoshino9.luogu.user.User
import org.jsoup.nodes.Element
import org.jsoup.nodes.Node

interface Comment {
	val user: User
	val date: String
	val content: List<Node>
}