package org.hoshino9.luogu.training

import org.hoshino9.luogu.user.User
import play.api.libs.json.{Json, Reads}

trait TrainingBase {
	def createTime: Long

	def deadline: Option[Int]

	def id: Int

	def markCount: Int

	def problemCount: Int

	def provider: User

	def title: String

	def `type`: Int
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