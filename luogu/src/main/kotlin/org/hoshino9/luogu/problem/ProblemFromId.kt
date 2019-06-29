package org.hoshino9.luogu.problem

import okhttp3.OkHttpClient
import org.hoshino9.luogu.LuoGuUtils
import org.hoshino9.luogu.problem.tags.parseTags
import org.hoshino9.luogu.tag.ColoredLuoGuTag
import org.hoshino9.luogu.user.User
import org.hoshino9.luogu.utils.*
import org.jsoup.nodes.Document
import org.jsoup.nodes.Element

open class ProblemFromId(override val id: String, client: OkHttpClient) : AbstractProblem(client), ProblemPage {
	companion object {
		private val regex = Regex(""" \w+ (.+)""")
	}

	val elem: Element by lazy {
		page.body()
	}

	private val right : Element by lazy {
		elem.getElementById("psummary")
	}

	private val ls : Element by lazy {
		right.child(0).child(0)
	}

	override val page : Document by lazy {
		client.page("${LuoGuUtils.baseUrl}/problemnew/show/$id")
	}

	override val content : ProblemContent by lazy {
		ProblemContent.parse(id, elem.getElementsByClass(ProblemContent.className).first() !!)
	}

	override val author : User by lazy {
		ls.child(1).children().last().child(0).attr("href").run(LuoGuUtils::userFromUrl)
	}

	override val difficulty : Problem.Difficulty by lazy {
		ls.child(4).run(::parseTags).first { it is Problem.Difficulty } as Problem.Difficulty
	}

	override val name : String by lazy {
		feInjection.get("currentMeta").asJsonObject.get("title").asString.run(regex::matchEntire) !!.groupValues[1]
	}

	override val passPercent : Pair<String, String> by lazy {
		ls.child(0).child(0).let {
			val first = it.child(0)
			val second = it.child(1)

			first.child(0).text() to second.child(0).text()
		}
	}

	override val tags: List<ColoredLuoGuTag> by lazy {
		ls.child(3).run(::parseTags)
	}
}