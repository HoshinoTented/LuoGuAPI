package org.hoshino9.luogu.record.response

import com.google.gson.annotations.SerializedName
import org.hoshino9.luogu.record.status.RecordStatus

data class RecordResponseBean(
		@SerializedName("_channel_param") override val rid : String,
		override val type : String,
		override val result : String,
		@SerializedName("welcome_message") override val recordStatus : RecordStatus,
		@SerializedName("client_number") override val clientNumber : Int
) : AbstractRecordResponse()