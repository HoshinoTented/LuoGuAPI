package org.hoshino9.luogu.problems

import org.hoshino9.luogu.utils.HasElement
import org.jsoup.nodes.Element

open class DefaultProblemContent(override val id : String) : AbstractProblemContent(), HasElement {
	override val background : Element
		get() = TODO("not implemented") //To change initializer of created properties use File | Settings | File Templates.
	override val description : Element
		get() = TODO("not implemented") //To change initializer of created properties use File | Settings | File Templates.
	override val inputFormat : Element
		get() = TODO("not implemented") //To change initializer of created properties use File | Settings | File Templates.
	override val outputFormat : Element
		get() = TODO("not implemented") //To change initializer of created properties use File | Settings | File Templates.
	override val inAndOut : List<Pair<String, String>>
		get() = TODO("not implemented") //To change initializer of created properties use File | Settings | File Templates.
	override val tip : Element
		get() = TODO("not implemented") //To change initializer of created properties use File | Settings | File Templates.
	override val elem : Element
		get() = TODO("not implemented") //To change initializer of created properties use File | Settings | File Templates.
}