package org.hoshino9.luogu.training

import org.hoshino9.luogu.*
import org.hoshino9.luogu.utils.*
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import org.jsoup.nodes.TextNode

open class DefaultTrainingPage(val luogu : LuoGu) : AbstractTrainingPage(), HasElement {
	override val elem : Element by lazy {
		luogu.executeGet("training/mainpage") { resp ->
			resp.assert()

			Jsoup.parse(resp.strData).body()
		}
	}

	private val right : Element by lazy {
		elem.getElementsByClass("am-list am-list-static lg-summary-list").first()
	}

	override val trainingBlocks : List<TrainingBlock> by lazy {
		val className = "lg-article"
		val attr = "traininglv"
		val mainBody = elem.getElementsByClass(className).last().childNodes()

		mainBody.splitWith {
			it.hasAttr(attr)
		}.mapNotNull {
			val elem = Element("Internal")
			it.forEach { node ->
				if (node is TextNode && node.isBlank) return@forEach        //会有空node, 需要排除, 不仅没有用还会引发异常
				node.clone()
				elem.appendChild(node)
			}

			if (elem.children().isEmpty()) null else TrainingBlock(elem, luogu)
		}
	}

	private val passedAndLast : Pair<String, String> by lazy {
		right.child(0).child(1).text().run(percentRegex::matchEntire) !!.groupValues.let { (_, a, b) ->
			a to b
		}
	}

	override val passedCount : String get() = passedAndLast.first
	override val lastPassedBlock : String get() = passedAndLast.second
	override val skipPercent : Pair<String, String> by lazy {
		right.child(1).child(1).text().run(percentRegex::matchEntire) !!.groupValues.let { (_, a, b) ->
			a to b
		}
	}
}