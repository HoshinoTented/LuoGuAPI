package org.hoshino9.luogu.record

import org.hoshino9.luogu.User
import org.hoshino9.luogu.interfaces.HasElement
import org.jsoup.nodes.Element

interface Record {
	enum class Status(val value : Int) {
		WAITING(0),
		JUDGING(1),
		COMPILE_ERROR(2),
		ACCEPTED(12),
		UNACCEPTED(14),
	}

	enum class SortMode {
		NEWEST,
		BEST
	}

	val rid : String
	val user : User
	val status : Record.Status
	val sort : Record.SortMode
}

abstract class AbstractRecord : Record {
	override fun equals(other : Any?) : Boolean {
		if (this === other) return true
		if (other !is AbstractRecord) return false

		return other.rid == rid
	}

	override fun hashCode() : Int {
		return rid.hashCode()
	}

	override fun toString() : String {
		return rid
	}
}

open class DefaultRecord(override val rid : String) : AbstractRecord(), HasElement {
	override val elem : Element by lazy {
		TODO()
	}

	override val user : User
		get() = TODO("not implemented") //To change initializer of created properties use File | Settings | File Templates.
	override val status : Record.Status
		get() = TODO("not implemented") //To change initializer of created properties use File | Settings | File Templates.
	override val sort : Record.SortMode
		get() = TODO("not implemented") //To change initializer of created properties use File | Settings | File Templates.

}

//@Suppress("unused")
//open class Record(val info : RecordInfo) {
//	data class RecordInfo(val pid : String, val user : User, val status : Status, val sort : SortMode) {
//		enum class Status(val value : Int) {
//			WAITING(0),
//			JUDGING(1),
//			COMPILE_ERROR(2),
//			ACCEPTED(12),
//			UNACCEPTED(14),
//		}
//
//		enum class SortMode {
//			NEWEST,
//			BEST
//		}
//	}
//}
