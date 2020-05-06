package org.hoshino9.luogu.problem

import com.google.gson.annotations.{JsonAdapter, SerializedName}
import org.hoshino9.luogu.json.JavaList
import org.hoshino9.luogu.json.Redirect

@JsonAdapter(classOf[Problem.Redirection])
trait Problem extends ProblemBase {
	val fullScore: Int

	val tags: JavaList[ProblemTag]

	val totalAccepted: Int

	val totalSubmit: Int

	val wantsTranslation: Boolean
}

object Problem {

	private[problem] class Redirection extends Redirect[Problem, Default]

	case class Default(difficulty: Difficulty,
	                   fullScore: Int,
	                   pid: ProblemID,
	                   tags: JavaList[ProblemTag],
	                   title: String,
	                   totalAccepted: Int,
	                   totalSubmit: Int,
	                   @SerializedName("type")
	                   problemType: ProblemType,
	                   wantsTranslation: Boolean) extends Problem

}