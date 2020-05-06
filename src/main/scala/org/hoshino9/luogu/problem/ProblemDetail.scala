package org.hoshino9.luogu.problem

import com.google.gson.{Gson, JsonObject}
import com.google.gson.annotations.{JsonAdapter, SerializedName}
import org.hoshino9.luogu.page.LuoGuClientPage
import org.hoshino9.luogu.{JavaList, LuoGuClient}
import org.hoshino9.luogu.user.User
import org.hoshino9.luogu.baseUrl
import org.hoshino9.luogu.json.Redirect
import org.hoshino9.luogu.problem.ProblemDetail.Limits

@JsonAdapter(classOf[ProblemDetail.Redirection])
trait ProblemDetail extends Problem {
	val accepted: Boolean
	val background: String
	val canEdit: Boolean
	val description: String
	val hint: String
	val inputFormat: String
	val outputFormat: String
	val limits: Limits
	val provider: User
	val samples: JavaList[AnyRef]
	val stdCode: String
}

object ProblemDetail {

	private[problem] class Redirection extends Redirect[ProblemDetail, Default]

	case class Limits(memory: JavaList[Int], time: JavaList[Int])

	case class Default(accepted: Boolean,
	                   background: String,
	                   canEdit: Boolean,
	                   description: String,
	                   difficulty: Int,
	                   fullScore: Int,
	                   hint: String,
	                   inputFormat: String,
	                   outputFormat: String,
	                   limits: Limits,
	                   pid: ProblemID,
	                   provider: User,
	                   samples: JavaList[AnyRef],
	                   stdCode: String,
	                   tags: JavaList[ProblemTag],
	                   title: String,
	                   totalAccepted: Int,
	                   totalSubmit: Int,
	                   @SerializedName("type")
	                   problemType: ProblemType,
	                   wantsTranslation: Boolean) extends ProblemDetail

	private class ProblemDetailPage(val pid: ProblemID, override val client: LuoGuClient) extends LuoGuClientPage {
		override val url: String = s"$baseUrl/problem/$pid"

		def problem: JsonObject = currentData.getAsJsonObject("problem")
	}

	implicit class RichLuoGuClient(val client: LuoGuClient) {
		def problem(id: ProblemID): ProblemDetail = {
			val page = new ProblemDetailPage(id, client)

			new Gson().fromJson(page.problem, classOf[ProblemDetail.Default])
		}
	}

}