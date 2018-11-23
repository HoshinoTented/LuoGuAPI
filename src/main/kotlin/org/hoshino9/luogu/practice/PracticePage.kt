package org.hoshino9.luogu.practice

import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.assert
import org.hoshino9.luogu.data
import org.hoshino9.luogu.getExecute
import org.hoshino9.luogu.interfaces.HasElement
import org.jsoup.Jsoup
import org.jsoup.nodes.Element

interface PracticePage {
	val practiceBlocks : List<PracticeBlock>
	val passedCount : String
	val lastPassedTime : String
	val skipPercent : Pair<String, String>
}

abstract class AbstractPracticePage : PracticePage {

}

open class DefaultPracticePage(val luogu : LuoGu) : AbstractPracticePage(), HasElement {
	override val elem : Element by lazy {
		luogu.getExecute("training/mainpage") { resp ->
			resp.assert()

			Jsoup.parse(resp.data).body()
		}
	}

	override val practiceBlocks : List<PracticeBlock>
		get() = TODO("not implemented") //To change initializer of created properties use File | Settings | File Templates.
	override val passedCount : String
		get() = TODO("not implemented") //To change initializer of created properties use File | Settings | File Templates.
	override val lastPassedTime : String
		get() = TODO("not implemented") //To change initializer of created properties use File | Settings | File Templates.
	override val skipPercent : Pair<String, String>
		get() = TODO("not implemented") //To change initializer of created properties use File | Settings | File Templates.

}