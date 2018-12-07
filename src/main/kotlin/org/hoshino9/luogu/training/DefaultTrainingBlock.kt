package org.hoshino9.luogu.training

import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.MatchException
import org.hoshino9.luogu.utils.HasElement
import org.jsoup.nodes.Element

open class DefaultTrainingBlock(override val elem : Element, val luogu : LuoGu)
	: AbstractTrainingBlock(), HasElement {
	companion object {
		private val passedPercentRegex = Regex("""(\d+) / (\d+) 达成""")
	}

	override val trainingLv : String by lazy {
		val attr = "traininglv"
		elem.child(0).attr(attr)
	}

	override val name : String by lazy {
		elem.child(0).text()
	}

	override val description : String by lazy {
		elem.childNode(1).toString().trim()
	}

	override val passPercent : Pair<String, String> by lazy {
		val tagName = "strong"
		val text = elem.getElementsByTag(tagName).first().text()

		passedPercentRegex.matchEntire(text)?.run {
			val first = groupValues[1]
			val second = groupValues[2]

			first to second
		} ?: throw MatchException(passedPercentRegex, text)
	}

	override val trainings : List<Training> by lazy {
		val tagName = "section"

		elem.getElementsByTag(tagName).map {
			val mid = it.attr("missionid")
			DefaultTraining(mid, luogu)
		}
	}
}