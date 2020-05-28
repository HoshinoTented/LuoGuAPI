package org.hoshino9.luogu.problem.solution

import org.hoshino9.luogu.{LuoGuClient, baseUrl}
import org.hoshino9.luogu.page.{ListPage, LuoGuClientPage}
import org.hoshino9.luogu.problem.ProblemID
import play.api.libs.json.{JsObject, JsResult, Json, Reads}

trait SolutionList extends ListPage {
	val result: Seq[Solution]
}

object SolutionList {
	implicit val reads: Reads[SolutionList] = Reads {
		Json.reads[Default].reads
	}

	case class Default(count: PostID, perPage: PostID, result: Seq[Solution]) extends SolutionList

	private class SolutionListPage(val pid: ProblemID, val page: Int, override val client: LuoGuClient) extends LuoGuClientPage {
		override val url: String = s"$baseUrl/problem/solution/$pid?page=$page"

		def solutions: JsObject = {
			currentData("solutions").as[JsObject]
		}
	}

	implicit class RichLuoGuClient(val client: LuoGuClient) {
		def solutions(pid: ProblemID, page: Int = 1): JsResult[SolutionList] = {
			val solutionPage = new SolutionListPage(pid, page, client)

			Json.fromJson(solutionPage.solutions)
		}
	}

}
