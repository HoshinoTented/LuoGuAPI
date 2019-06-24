package org.hoshino9.luogu.record.status

data class RecordStatusBean(
		override val status : RecordStatus.Status,
		override val memory: String?,
		override val score: String?,
		override val time : String,
		override val detail : RecordStatus.Detail
) : AbstractRecordStatus()