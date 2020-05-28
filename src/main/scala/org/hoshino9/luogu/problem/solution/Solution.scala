package org.hoshino9.luogu.problem.solution

import org.hoshino9.luogu.user.User
import play.api.libs.json.{Json, Reads}

trait Solution {
	val author: User
	val commentCount: Int
	val content: String
	val contentDescription: String
	val currentUserVoteType: Int
	val id: PostID
	val identifier: String
	val postTime: Long
	val status: Int
	val thumbUp: Int
	val title: String
	val `type`: PostType
}

object Solution {
	implicit val reads: Reads[Solution] = Reads {
		Json.reads[Default].reads
	}

	case class Default(author: User,
	                   commentCount: Int,
	                   content: String,
	                   contentDescription: String,
	                   currentUserVoteType: Int,
	                   id: PostID,
	                   identifier: String,
	                   postTime: Long,
	                   status: Int,
	                   thumbUp: Int,
	                   title: String,
	                   `type`: PostType) extends Solution

}