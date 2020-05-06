package org.hoshino9.luogu.problem

import com.google.gson.annotations.{JsonAdapter, SerializedName}
import org.hoshino9.luogu.json.Redirect

@JsonAdapter(classOf[ProblemBase.Redirection])
trait ProblemBase {
	val difficulty: Difficulty
	val pid: ProblemID
	val title: String

	@SerializedName("type")
	val problemType: ProblemType
}

object ProblemBase {

	private[problem] class Redirection extends Redirect[ProblemBase, Default]

	case class Default(difficulty: Difficulty,
	                   pid: ProblemID,
	                   title: String,
	                   problemType: ProblemType) extends ProblemBase

}