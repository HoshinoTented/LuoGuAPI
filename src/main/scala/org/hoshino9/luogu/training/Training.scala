package org.hoshino9.luogu.training

import com.google.gson.{Gson, JsonObject}
import com.google.gson.annotations.JsonAdapter
import org.hoshino9.luogu.{LuoGuClient, baseUrl}
import org.hoshino9.luogu.json.{JavaList, Redirect}
import org.hoshino9.luogu.page.LuoGuClientPage
import org.hoshino9.luogu.problem.{Difficulty, Problem, ProblemBase, ProblemID, ProblemTag, ProblemType}
import org.hoshino9.luogu.training.Training.ProblemWrapper
import org.hoshino9.luogu.user.User

@JsonAdapter(classOf[Training.Redirection])
trait Training extends TrainingBase {
	val description: String
	val problems: JavaList[ProblemWrapper]
}

object Training {

	private[luogu] class Redirection extends Redirect[Training, Default]

	class ProblemWrapper(val problem: Problem)

	implicit def unwrap(wrapper: ProblemWrapper): Problem = wrapper.problem

	case class Default(description: String,
	                   problems: JavaList[ProblemWrapper],
	                   createTime: Long,
	                   deadline: Option[Integer],
	                   id: Difficulty,
	                   markCount: Difficulty,
	                   problemCount: Difficulty,
	                   provider: User,
	                   title: String,
	                   `type`: Int) extends Training

	private class TrainingPage(val id: Int, override val client: LuoGuClient) extends LuoGuClientPage {
		override val url: String = s"$baseUrl/training/$id"

		def training: JsonObject = {
			currentData.getAsJsonObject("training")
		}
	}

	implicit class RichLuoGuClient(val client: LuoGuClient) extends AnyVal {
		def training(id: Int): Training = {
			val training = new TrainingPage(id, client).training

			new Gson().fromJson(training, classOf[Training])
		}
	}

}