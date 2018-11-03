package org.hoshino9.luogu.practice

import org.hoshino9.luogu.problems.Problem

interface Practice {
	val mid : String
	val name : String
	val passed : Boolean
	val problems : List<Problem>
}

abstract class AbstractPractice : Practice {
	override fun toString() : String {
		return mid
	}

	override fun equals(other : Any?) : Boolean {
		return (other as? AbstractPractice)?.mid == mid
	}

	override fun hashCode() : Int {
		return mid.hashCode()
	}
}