package org.hoshino9.luogu.training

import org.hoshino9.luogu.page.LuoGuClientPage
import org.hoshino9.luogu.problem.{Difficulty, Problem}
import org.hoshino9.luogu.training.Training.ProblemWrapper
import org.hoshino9.luogu.user.User
import org.hoshino9.luogu.{LuoGuClient, baseUrl}
import play.api.libs.json.{JsObject, JsResult, Json, Reads}

trait Training extends TrainingBase {
	val description: String
	val problems: Seq[ProblemWrapper]
}

object Training {

	case class ProblemWrapper(problem: Problem)

	object ProblemWrapper {
		implicit val reads: Reads[ProblemWrapper] = Json.reads
	}

	implicit val reads: Reads[Training] = Reads {
		Json.reads[Default].reads
	}

	implicit def unwrap(wrapper: ProblemWrapper): Problem = wrapper.problem

	case class Default(description: String,
	                   problems: Seq[ProblemWrapper],
	                   createTime: Long,
	                   deadline: Option[Int],
	                   id: Difficulty,
	                   markCount: Difficulty,
	                   problemCount: Difficulty,
	                   provider: User,
	                   title: String,
	                   `type`: Int) extends Training

	private class TrainingPage(val id: Int, override val client: LuoGuClient) extends LuoGuClientPage {
		override val url: String = s"$baseUrl/training/$id"

		def training: JsObject = {
			currentData("training").as[JsObject]
		}
	}

	implicit class RichLuoGuClient(val client: LuoGuClient) extends AnyVal {
		def training(id: Int): JsResult[Training] = {
			val training = new TrainingPage(id, client).training

			Json.fromJson(training)
		}
	}

}