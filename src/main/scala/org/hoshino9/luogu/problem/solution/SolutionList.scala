package org.hoshino9.luogu.problem.solution

import com.google.gson.annotations.JsonAdapter
import com.google.gson.{Gson, JsonObject}
import org.hoshino9.luogu.baseUrl
import org.hoshino9.luogu.{JavaList, LuoGuClient}
import org.hoshino9.luogu.json.Redirect
import org.hoshino9.luogu.page.{ListPage, LuoGuClientPage}
import org.hoshino9.luogu.problem.ProblemID

@JsonAdapter(classOf[SolutionList.Redirection])
trait SolutionList extends ListPage {
	val result: JavaList[Solution]
}

object SolutionList {

	private[solution] class Redirection extends Redirect[SolutionList, Default]

	case class Default(count: PostID, perPage: PostID, result: JavaList[Solution]) extends SolutionList

	private class SolutionListPage(val pid: ProblemID, val page: Int, override val client: LuoGuClient) extends LuoGuClientPage {
		override val url: String = s"$baseUrl/problem/solution/$pid?page=$page"

		def solutions: JsonObject = {
			currentData.getAsJsonObject("solutions")
		}
	}

	implicit class RichLuoGuClient(val client: LuoGuClient) {
		def solutionList(pid: ProblemID, page: Int = 1): SolutionList = {
			val solutionPage = new SolutionListPage(pid, page, client)

			new Gson().fromJson(solutionPage.solutions, classOf[SolutionList])
		}
	}

}
