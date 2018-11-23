package org.hoshino9.luogu.problems

import org.hoshino9.luogu.interfaces.HasElement
import org.jsoup.nodes.Element

interface ProblemContent {
	/**
	 * 题目id
	 */
	val id : String

	/**
	 * 题目背景
	 */
	val background : Element

	/**
	 * 题目描述
	 */
	val description : Element

	/**
	 * 输入格式
	 */
	val inputFormat : Element

	/**
	 * 输出格式
	 */
	val outputFormat : Element

	/**
	 * 输入和输出的样例列表<br>
	 * Pair.first 是输入<br>
	 * Pair.second 是输出
	 */
	val inAndOut : List<Pair<String, String>>

	/**
	 * 题目提示
	 */
	val tip : Element
}

abstract class AbstractProblemContent : ProblemContent {
	override fun equals(other : Any?) : Boolean {
		if (this === other) return true
		if (javaClass != other?.javaClass) return false

		other as AbstractProblemContent

		return other.id == id
	}

	override fun hashCode() : Int {
		return id.hashCode()
	}
}

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