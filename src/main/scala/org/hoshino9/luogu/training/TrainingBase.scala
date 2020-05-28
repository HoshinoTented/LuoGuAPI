package org.hoshino9.luogu.training

import org.hoshino9.luogu.user.User
import play.api.libs.json.{Json, Reads}

trait TrainingBase {
	val createTime: Long
	val deadline: Option[Int]
	val id: Int
	val markCount: Int
	val problemCount: Int
	val provider: User
	val title: String
	val `type`: Int
}

object TrainingBase {
	implicit val reads: Reads[TrainingBase] = Reads {
		Json.reads[Default].reads
	}

	case class Default(createTime: Long,
	                   deadline: Option[Int],
	                   id: Int,
	                   markCount: Int,
	                   problemCount: Int,
	                   provider: User,
	                   title: String,
	                   `type`: Int) extends TrainingBase {
	}

}