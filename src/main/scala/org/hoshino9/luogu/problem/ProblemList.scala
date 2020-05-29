package org.hoshino9.luogu.problem

import org.hoshino9.luogu.page.{ListPage, LuoGuClientPage}
import org.hoshino9.luogu.{LuoGuClient, baseUrl}
import play.api.libs.json.{JsObject, JsResult, Json, Reads}

trait ProblemList extends ListPage {
	def result: Seq[Problem]
}

object ProblemList {
	implicit val reads: Reads[ProblemList] = Reads {
		Json.reads[Default].reads
	}

	case class Default(override val count: Int,
	                   override val perPage: Int,
	                   result: Seq[Problem]) extends ProblemList

	private class ProblemListPage(val page: Int, override val client: LuoGuClient) extends LuoGuClientPage {
		override val url: String = s"$baseUrl/problem/list?page=$page"

		def problems: JsObject = currentData("problems").as[JsObject]
	}

	implicit class RichLuoGuClient(val client: LuoGuClient) extends AnyVal {
		def problems(page: Int = 1): JsResult[ProblemList] = {
			val problemPage = new ProblemListPage(page, client)

			Json.fromJson(problemPage.problems)
		}
	}

}