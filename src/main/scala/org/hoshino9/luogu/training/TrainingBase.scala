package org.hoshino9.luogu.training

import com.google.gson.annotations.{JsonAdapter, SerializedName}
import org.hoshino9.luogu.json.Redirect
import org.hoshino9.luogu.user.User

@JsonAdapter(classOf[TrainingBase.Redirection])
trait TrainingBase {
	val createTime: Long
	val deadline: Option[Integer]
	val id: Int
	val markCount: Int
	val problemCount: Int
	val provider: User
	val title: String
	val `type`: Int
}

object TrainingBase {

	private[luogu] class Redirection extends Redirect[TrainingBase, Default]

	case class Default(createTime: Long,
	                   @SerializedName("deadline")
	                   private[luogu] val _deadline: Integer,
	                   id: Int,
	                   markCount: Int,
	                   problemCount: Int,
	                   provider: User,
	                   title: String,
	                   @SerializedName("type")
	                   `type`: Int) extends TrainingBase {
		override lazy val deadline: Option[Integer] = {
			Option(_deadline)
		}
	}

}