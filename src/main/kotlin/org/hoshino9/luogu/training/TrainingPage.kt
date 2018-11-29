package org.hoshino9.luogu.training

import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.assert
import org.hoshino9.luogu.data
import org.hoshino9.luogu.getExecute
import org.hoshino9.luogu.interfaces.HasElement
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import org.hoshino9.luogu.splitWith
import org.jsoup.nodes.TextNode

interface TrainingPage {
	companion object {
		@JvmName("newInstance")
		operator fun invoke(luogu : LuoGu) : TrainingPage {
			return DefaultTrainingPage(luogu)
		}
	}

	val trainingBlocks : List<TrainingBlock>
	val passedCount : String
	val lastPassedTime : String
	val skipPercent : Pair<String, String>
}

abstract class AbstractTrainingPage : TrainingPage

open class DefaultTrainingPage(val luogu : LuoGu) : AbstractTrainingPage(), HasElement {
	override val elem : Element by lazy {
		luogu.getExecute("training/mainpage") { resp ->
			resp.assert()

			Jsoup.parse(resp.data).body()
		}
	}

	override val trainingBlocks : List<TrainingBlock> by lazy {
		val className = "lg-article"
		val attr = "traininglv"
		val mainBody = elem.getElementsByClass(className).last().childNodes()

		mainBody.splitWith {
			it.hasAttr(attr)
		}.mapNotNull { it ->
			val elem = Element("Internal")
			it.forEach { node ->
				if (node is TextNode && node.isBlank) return@forEach        //会有空node, 需要排除, 不仅没有用还会引发异常
				node.clone()
				elem.appendChild(node)
			}

			if (elem.children().isEmpty()) null else TrainingBlock(elem, luogu)
		}
	}
	override val passedCount : String
		get() = TODO("not implemented") //To change initializer of created properties use File | Settings | File Templates.
	override val lastPassedTime : String
		get() = TODO("not implemented") //To change initializer of created properties use File | Settings | File Templates.
	override val skipPercent : Pair<String, String>
		get() = TODO("not implemented") //To change initializer of created properties use File | Settings | File Templates.
}