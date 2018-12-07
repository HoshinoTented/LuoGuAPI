package org.hoshino9.luogu.problems

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