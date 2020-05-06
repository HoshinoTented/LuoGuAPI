package org.hoshino9.luogu.problem

import com.google.gson.annotations.JsonAdapter
import com.google.gson.{Gson, JsonObject}
import org.hoshino9.luogu.json.{JavaList, Redirect}
import org.hoshino9.luogu.{LuoGuClient, baseUrl}
import org.hoshino9.luogu.page.{ListPage, LuoGuClientPage}

@JsonAdapter(classOf[ProblemList.Redirection])
trait ProblemList extends ListPage {
	val result: JavaList[Problem]
}

object ProblemList {

	private[problem] class Redirection extends Redirect[ProblemList, Default]

	case class Default(override val count: Int,
	                   override val perPage: Int,
	                   result: JavaList[Problem]) extends ProblemList

	private class ProblemListPage(val page: Int, override val client: LuoGuClient) extends LuoGuClientPage {
		override val url: String = s"$baseUrl/problem/list?page=$page"

		def problems: JsonObject = currentData.getAsJsonObject("problems")
	}

	implicit class RichLuoGuClient(val client: LuoGuClient) extends AnyVal {
		def problemList(page: Int = 1): ProblemList = {
			val problemPage = new ProblemListPage(page, client)

			new Gson().fromJson(problemPage.problems, classOf[ProblemList])
		}
	}

}