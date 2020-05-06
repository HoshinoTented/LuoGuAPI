package org.hoshino9.luogu.user

import com.google.gson.annotations.JsonAdapter
import org.hoshino9.luogu.json.Redirect

@JsonAdapter(classOf[User.Redirection])
trait User {
	val badge: String
	val ccfLevel: Int
	val color: String
	val isAdmin: Boolean
	val isBanned: Boolean
	val name: String
	val slogan: String
	val uid: Int
}

object User {

	private[user] class Redirection extends Redirect[User, Default]

	case class Default(badge: String,
	                   ccfLevel: Int,
	                   color: String,
	                   isAdmin: Boolean,
	                   isBanned: Boolean,
	                   name: String,
	                   slogan: String,
	                   uid: Int) extends User

}