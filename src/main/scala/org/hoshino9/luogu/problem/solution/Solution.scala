package org.hoshino9.luogu.problem.solution

import com.google.gson.annotations.{JsonAdapter, SerializedName}
import org.hoshino9.luogu.LuoGuClient
import org.hoshino9.luogu.json.Redirect
import org.hoshino9.luogu.user.User

@JsonAdapter(classOf[Solution.Redirection])
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
	@SerializedName("type")
	val postType: PostType
}

object Solution {

	private[solution] class Redirection extends Redirect[Solution, Default]

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
	                   @SerializedName("type")
	                   postType: PostType) extends Solution

}