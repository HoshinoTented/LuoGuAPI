package org.hoshino9.luogu.training

import org.hoshino9.luogu.*
import org.hoshino9.luogu.problem.experimental.Problem
import org.hoshino9.luogu.utils.*
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import java.lang.IllegalArgumentException

open class DefaultTraining(override val mid: String, val luogu: LuoGu) : AbstractTraining() {
	val elem: Element by lazy {
		luogu.executeGet("training/ajax_get_detail?missionid=$mid") { resp ->
			resp.assert()
			val content = resp.strData

			if (content.isEmpty()) throw HTMLParseException(null, "no data")

			json(content) {
				val code: Int? by delegate

				if (code != 200) throw IllegalStatusCodeException(code, get("message").asString)
				Jsoup.parse(getAsJsonObject("more").get("html").asString).body()
			}
		}
	}

	override val name : String
		get() {
			return elem.child(0).textNodes().first().text().trim()
		}

	override val status : Training.Status
		get() {
			val className = "am-badge"

			return elem.getElementsByClass(className).first().text().run {
				Training.Status.values().firstOrNull { it.content == this } ?: throw IllegalArgumentException("No such status: $this")
			}
		}

	override val problems : List<Problem>
		get() {
			val className = "colored"

			return elem.getElementsByClass(className).map {
				val id = it.child(0).text()

				Problem.Factory(id, luogu.client)
			}
		}

	override fun equals(other : Any?) : Boolean {
		if (this === other) return true
		if (other !is DefaultTraining) return false

		if (mid != other.mid) return false

		return true
	}

	override fun hashCode() : Int {
		return mid.hashCode()
	}
}