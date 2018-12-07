package org.hoshino9.luogu.paste

import org.hoshino9.luogu.user.User

interface Paste {
	val id : String
	val user : User
	val url : String
	val date : String
	val source : String
	val isPublic : Boolean
}