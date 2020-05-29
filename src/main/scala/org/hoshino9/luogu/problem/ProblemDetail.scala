package org.hoshino9.luogu.problem

import org.hoshino9.luogu.{LuoGuClient, baseUrl}
import org.hoshino9.luogu.page.LuoGuClientPage
import org.hoshino9.luogu.problem.ProblemDetail.Limits
import org.hoshino9.luogu.user.User
import play.api.libs.json.{JsObject, JsResult, Json, Reads}

trait ProblemDetail extends Problem {
	def background: String

	def canEdit: Boolean

	def description: String

	def hint: String

	def inputFormat: String

	def outputFormat: String

	def limits: Limits

	def provider: User

	//	val samples: Seq[AnyRef]
	def stdCode: String
}

object ProblemDetail {

	case class Limits(memory: Seq[Int], time: Seq[Int])

	object Limits {
		implicit val reads: Reads[Limits] = Json.reads[Limits]
	}

	implicit val reads: Reads[ProblemDetail] = Reads {
		Json.reads[Default].reads
	}

	case class Default(background: String,
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
	                   //	                   samples: Seq[AnyRef],
	                   stdCode: String,
	                   tags: Seq[ProblemTag],
	                   title: String,
	                   totalAccepted: Int,
	                   totalSubmit: Int,
	                   `type`: ProblemType,
	                   wantsTranslation: Boolean) extends ProblemDetail

	private class ProblemDetailPage(val pid: ProblemID, override val client: LuoGuClient) extends LuoGuClientPage {
		override val url: String = s"$baseUrl/problem/$pid"

		def problem: JsObject = currentData("problem").as[JsObject]
	}

	implicit class RichLuoGuClient(val client: LuoGuClient) {
		def problem(id: ProblemID): JsResult[ProblemDetail] = {
			val page = new ProblemDetailPage(id, client)

			Json.fromJson(page.problem)
		}
	}

}