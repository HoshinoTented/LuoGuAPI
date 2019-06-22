package org.hoshino9.luogu.record.response

import org.hoshino9.luogu.record.status.RecordStatus

data class RecordResponseBean(
		override val rid : String,
		override val type : String,
		override val result : String,
		override val recordStatus : RecordStatus,
		override val clientNumber : Int
) : AbstractRecordResponse()