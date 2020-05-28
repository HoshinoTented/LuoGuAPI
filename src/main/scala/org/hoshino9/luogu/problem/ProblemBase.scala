package org.hoshino9.luogu.problem

import play.api.libs.json.{Json, Reads}

trait ProblemBase {
	val difficulty: Difficulty
	val pid: ProblemID
	val title: String
	val `type`: ProblemType
}

object ProblemBase {
	implicit val reads: Reads[ProblemBase] = Reads {
		Json.reads[Default].reads
	}

	case class Default(difficulty: Difficulty,
	                   pid: ProblemID,
	                   title: String,
	                   `type`: ProblemType) extends ProblemBase

}