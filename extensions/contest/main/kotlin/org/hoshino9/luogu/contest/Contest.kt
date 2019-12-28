package org.hoshino9.luogu.contest

import com.google.gson.JsonDeserializationContext
import com.google.gson.JsonDeserializer
import com.google.gson.JsonElement
import com.google.gson.annotations.JsonAdapter
import org.hoshino9.luogu.user.BaseUser
import org.hoshino9.luogu.utils.Deserializable
import org.hoshino9.luogu.utils.delegate
import java.lang.reflect.Type

@JsonAdapter(Host.Serializer::class)
sealed class Host {
	companion object Serializer : JsonDeserializer<Host> {
		override fun deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): Host {
			val source = json.asJsonObject
			val host: Host = source.let { host ->
				if (host.has("id")) Organization(host["id"].asInt, host["name"].asString) else {
					User(context.deserialize(host, BaseUser::class.java))
				}
			}

			return host
		}
	}

	abstract val id: Int
	abstract val name: String

	data class User(val user: BaseUser) : Host() {
		override val id: Int
			get() = user.uid

		override val name: String
			get() = user.name
	}

	data class Organization(override val id: Int, override val name: String) : Host()
}

@JsonAdapter(BaseContestImpl.Serializer::class)
interface BaseContest {
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

data class BaseContestImpl(override val id: Int, override val name: String, override val host: Host, override val problemCount: Int, override val rated: Boolean, override val ruleType: RuleType, override val visibilityType: VisibilityType, override val startTime: Long, override val endTime: Long) : BaseContest {
	companion object Serializer : Deserializable<BaseContest>(BaseContest::class), JsonDeserializer<BaseContest> {
		override fun deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): BaseContest {
			return context.deserialize(json, BaseContestImpl::class.java)
		}
	}

	override fun equals(other: Any?): Boolean {
		return (other as? BaseContest)?.id == this.id
	}

	override fun hashCode(): Int {
		return id.hashCode()
	}
}

@JsonAdapter(ContestImpl.Serializer::class)
interface Contest : BaseContest {
	/**
	 * 比赛介绍
	 */
	val description: String

	/**
	 * 比赛总参加人数
	 */
	val totalParticipants: Int
}

data class ContestImpl(override val description: String, override val totalParticipants: Int, val baseContest: BaseContest) : BaseContest by baseContest, Contest {
	companion object Serializer : Deserializable<Contest>(Contest::class), JsonDeserializer<Contest> {
		override fun deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): Contest {
			val source = json.asJsonObject
			val delegate = source.delegate

			val description: String by delegate
			val totalParticipants: Int by delegate
			val baseContest: BaseContest = context.deserialize(json, BaseContest::class.java)

			return ContestImpl(description, totalParticipants, baseContest)
		}
	}

	override fun equals(other: Any?): Boolean {
		return baseContest == other
	}

	override fun hashCode(): Int {
		return baseContest.hashCode()
	}
}