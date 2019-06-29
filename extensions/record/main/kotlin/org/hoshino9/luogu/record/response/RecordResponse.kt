package org.hoshino9.luogu.record.response

import com.google.gson.JsonObject
import org.hoshino9.luogu.record.status.RecordStatus
import org.hoshino9.luogu.utils.delegate
import org.hoshino9.luogu.utils.json

interface RecordResponse {
	val rid : String
	val type : String
	val result : String
	val recordStatus : RecordStatus
	val clientNumber : Int

	companion object {
		@JvmName("newInstance")
		operator fun invoke(src : String) : RecordResponse {
			return json(src) {
				val property = delegate
				val _channel_param : String by property
				val type : String by property
				val result : String by property
				val welcome_message: JsonObject by property
				val client_number : Int by property


				RecordResponseBean(_channel_param, type, result, welcome_message.run(RecordStatus.Companion::invoke), client_number)
			}
		}
	}
}