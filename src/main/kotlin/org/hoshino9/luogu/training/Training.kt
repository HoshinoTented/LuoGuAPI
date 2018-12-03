@file:Suppress("unused")

package org.hoshino9.luogu.training

import org.hoshino9.luogu.*
import org.hoshino9.luogu.interfaces.HasElement
import org.hoshino9.luogu.problems.Problem
import org.hoshino9.luogu.problems.ProblemFromId
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import java.lang.IllegalArgumentException

interface Training {
	enum class Status(val content : String) {
		ALL_KILL("已通过"),
		CHALLENGING("可挑战"),
		DISABLE("有先决要求"),
		SKIP("暂时跳过"),
	}

	companion object {
		@JvmName("newInstance")
		operator fun invoke(mid : String, luogu : LuoGu) : Training {
			return DefaultTraining(mid, luogu)
		}
	}

	val mid : String
	val name : String
	val status : Status
	val problems : List<Problem>
}

abstract class AbstractTraining : Training {
	override fun toString() : String {
		return mid
	}
}

open class DefaultTraining(override val mid : String, val luogu : LuoGu) : AbstractTraining(), HasElement {
	override val elem : Element by lazy {
		luogu.getExecute("training/ajax_get_detail?missionid=$mid") { resp ->
			resp.assert()
			val content = resp.data

			if (content == null || content.isEmpty()) throw HTMLParseException(null, "no data")

			json(content) {
				val code = getInt("code")

				if (code != 200) throw IllegalStatusCodeException(code, getString("message"))
				Jsoup.parse(getJSONObject("more").getString("html")).body()
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

				ProblemFromId(id, luogu.client)
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