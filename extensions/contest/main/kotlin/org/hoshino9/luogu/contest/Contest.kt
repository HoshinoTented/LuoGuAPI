package org.hoshino9.luogu.contest

import com.google.gson.JsonObject
import org.hoshino9.luogu.utils.delegate

sealed class Host {
	data class User(val uid: Int) : Host()
	data class Organization(val id: Int) : Host()
}

interface IBaseContest {
	val id: Int
	val name: String
	val host: Host
	val problemCount: Int
	val rated: Boolean
	val ruleType: RuleType
	val visibilityType: VisibilityType
	val startTime: Long
	val endTime: Long
}

open class BaseContest(protected val source: JsonObject) : IBaseContest {
	protected val delegate = source.delegate

	override val id: Int by delegate
	override val name: String by delegate
	override val problemCount: Int by delegate
	override val rated: Boolean by delegate
	override val startTime: Long by delegate
	override val endTime: Long by delegate

	override val host: Host
		get() {
			return source["host"].asJsonObject.let { host ->
				if (host.has("id")) Host.Organization(host["id"].asInt) else {
					Host.User(host["uid"].asInt)
				}
			}
		}

	override val ruleType: RuleType
		get() = RuleType.values()[source["ruleType"].asInt]

	override val visibilityType: VisibilityType
		get() = VisibilityType.values()[source["visibilityType"].asInt]
}

interface IContest : IBaseContest {
	val description: String
	val totalParticipants: Int
}

open class Contest(source: JsonObject) : BaseContest(source), IContest {
	override val description: String by delegate
	override val totalParticipants: Int by delegate
}