package org.hoshino9.luogu.record

import com.google.gson.annotations.SerializedName
import org.hoshino9.luogu.globalGson

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

abstract class AbstractRecordResponse : RecordResponse

data class RecordResponseBean(
		@SerializedName("_channel_param") override val rid : String,
		override val type : String,
		override val result : String,
		@SerializedName("welcome_message") override val recordStatus : RecordStatus,
		@SerializedName("client_number") override val clientNumber : Int
) : AbstractRecordResponse()