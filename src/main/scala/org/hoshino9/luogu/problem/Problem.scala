package org.hoshino9.luogu.problem

import play.api.libs.json.{Json, Reads}

trait Problem extends ProblemBase {
	val fullScore: Int

	val tags: Seq[ProblemTag]

	val totalAccepted: Int

	val totalSubmit: Int

	val wantsTranslation: Boolean
}

object Problem {
	implicit val reader: Reads[Problem] = Reads {
		Json.reads[Default].reads
	}

	case class Default(difficulty: Difficulty,
	                   fullScore: Int,
	                   pid: ProblemID,
	                   tags: Seq[ProblemTag],
	                   title: String,
	                   totalAccepted: Int,
	                   totalSubmit: Int,
	                   `type`: ProblemType,
	                   wantsTranslation: Boolean) extends Problem

}