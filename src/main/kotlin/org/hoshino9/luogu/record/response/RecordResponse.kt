package org.hoshino9.luogu.record.response

import org.hoshino9.luogu.globalGson
import org.hoshino9.luogu.record.status.RecordStatus

interface RecordResponse {
	val rid : String
	val type : String
	val result : String
	val recordStatus : RecordStatus
	val clientNumber : Int

	companion object {
		@JvmName("newInstance")
		operator fun invoke(json : String) : RecordResponse {
			return globalGson.fromJson(json, RecordResponseBean::class.java)
		}
	}
}