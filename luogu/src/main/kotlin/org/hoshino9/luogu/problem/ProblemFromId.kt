package org.hoshino9.luogu.problem

import okhttp3.OkHttpClient
import org.hoshino9.luogu.LuoGuUtils
import org.hoshino9.luogu.tag.LuoGuTag
import org.hoshino9.luogu.utils.*
import org.jsoup.Jsoup
import org.jsoup.nodes.Element

open class ProblemFromId(override val id : String, val client : OkHttpClient) : AbstractProblem(), HasElement {
	override val elem : Element by lazy {
		client.executeGet("${LuoGuUtils.baseUrl}/problemnew/show/$id") {
			it.assert()

			Jsoup.parse(it.strData)
		}
	}

	override val content : ProblemContent by lazy {
		ProblemContent.parse(id, elem.getElementsByClass(ProblemContent.className).first() !!)
	}

	override val difficulty : Problem.Difficulty
		get() = TODO("not implemented") //To change initializer of created properties use File | Settings | File Templates.
	override val name : String
		get() = TODO("not implemented") //To change initializer of created properties use File | Settings | File Templates.
	override val passPercent : Pair<String, String>
		get() = TODO("not implemented") //To change initializer of created properties use File | Settings | File Templates.
	override val tags : List<LuoGuTag>
		get() = TODO("not implemented") //To change initializer of created properties use File | Settings | File Templates.
}