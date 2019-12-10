package org.hoshino9.luogu.contest

import com.google.gson.JsonObject
import org.hoshino9.luogu.utils.delegate

sealed class Host {
	data class User(val uid: Int) : Host()
	data class Organization(val id: Int) : Host()
}

interface IBaseContest {
	/**
	 * 比赛 ID
	 */
	val id: Int

	/**
	 * 比赛名次
	 */
	val name: String

	/**
	 * 比赛举办者
	 */
	val host: Host

	/**
	 * 比赛题目数量
	 */
	val problemCount: Int

	/**
	 * 是否 Rated
	 */
	val rated: Boolean

	/**
	 * 比赛规则类型
	 */
	val ruleType: RuleType

	/**
	 * 比赛类型（可见类型）
	 */
	val visibilityType: VisibilityType

	/**
	 * 比赛开始时间（时间戳）
	 */
	val startTime: Long

	/**
	 * 比赛结束时间（时间戳）
	 */
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
	/**
	 * 比赛介绍
	 */
	val description: String

	/**
	 * 比赛总参加人数
	 */
	val totalParticipants: Int
}

open class Contest(source: JsonObject) : BaseContest(source), IContest {
	override val description: String by delegate
	override val totalParticipants: Int by delegate
}