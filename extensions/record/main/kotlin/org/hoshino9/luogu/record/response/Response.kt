package org.hoshino9.luogu.record.response

import com.google.gson.*
import com.google.gson.annotations.JsonAdapter
import org.hoshino9.luogu.utils.delegate
import org.hoshino9.luogu.utils.gson
import java.lang.reflect.Type

@JsonAdapter(Response.Deserializer::class)
data class Response(
		val status: Status,
		val compileMessage: String,
		val testCases: Map<String, TestCase>) {
	object Deserializer : JsonDeserializer<Response> {
		override fun deserialize(json: JsonElement, typeOfT: Type?, context: JsonDeserializationContext?): Response {
			return json.asJsonObject.delegate.let { json ->
				val record: JsonObject by json
				val detail: JsonObject by record.delegate

				val status: Status = gson.fromJson(record["status"], Status::class.java)
				val compileMessage = detail["compile"].asJsonObject["content"].asString
				val testCases = LinkedHashMap<String, TestCase>()

				detail["testcases"]?.asJsonObject?.let { testcases ->
					testcases.keySet().forEach {
						testCases[it] = gson.fromJson(testcases[it], TestCase::class.java)
					}
				}

				Response(status, compileMessage, testCases)
			}
		}
	}

	object StatusDeserializer : JsonDeserializer<Status> {
		override fun deserialize(json: JsonElement, typeOfT: Type?, context: JsonDeserializationContext?): Status {
			return json.asInt.let { id ->
				Status.values().first { it.id == id }
			}
		}
	}

	@JsonAdapter(StatusDeserializer::class)
	enum class Status(val id: Int, val fullName: String) {
		Waiting(0, "Waiting"),
		Judging(1, "Judging"),
		CE(2, "Compile Error"),
		OLE(3, "Output Limit Exceeded"),
		MLE(4, "Memory Limit Exceeded"),
		TLE(5, "Time Limit Exceeded"),
		WA(6, "Wrong Answer"),
		RE(7, "Runtime Error"),
		UKE(11, "Unknown Error"),
		AC(12, "Accepted"),
		Unaccepted(14, "Unaccepted"),
		HackSuccess(21, "Hack Success"),
		HackFailure(22, "Hack Failure"),
		HackSkipped(23, "Hack Skipped"),
		Unshown(- 1, "Unshown")
	}
}