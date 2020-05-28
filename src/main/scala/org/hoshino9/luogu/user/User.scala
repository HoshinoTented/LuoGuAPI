package org.hoshino9.luogu.user

import play.api.libs.json.{Json, Reads}

trait User {
	val badge: Option[String]
	val ccfLevel: Int
	val color: String
	val isAdmin: Boolean
	val isBanned: Boolean
	val name: String
	val slogan: String
	val uid: Int
}

object User {
	implicit val reads: Reads[User] = Reads {
		Json.reads[Default].reads
	}

	case class Default(badge: Option[String],
	                   ccfLevel: Int,
	                   color: String,
	                   isAdmin: Boolean,
	                   isBanned: Boolean,
	                   name: String,
	                   slogan: String,
	                   uid: Int) extends User

}