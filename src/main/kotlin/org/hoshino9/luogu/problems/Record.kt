package org.hoshino9.luogu.problems

import org.hoshino9.luogu.LuoGuUser

@Suppress("unused")
class Record(val info : RecordInfo) {
	data class RecordInfo(val pid : String, val user : LuoGuUser, val status : Status, val sort : SortMode) {
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
	}
}
