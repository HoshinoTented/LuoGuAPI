package org.hoshino9.luogu.training

import com.google.gson.*
import com.google.gson.annotations.JsonAdapter
import org.hoshino9.luogu.problem.BaseProblem
import org.hoshino9.luogu.problem.BaseProblemImpl
import org.hoshino9.luogu.user.BaseUser
import org.hoshino9.luogu.user.ProblemID
import org.hoshino9.luogu.utils.Deserializable
import org.hoshino9.luogu.utils.delegate
import java.lang.reflect.Type

@JsonAdapter(BaseTrainingImpl.Serializer::class)
interface BaseTraining {
	companion object;

	/**
	 * 创建时间
	 */
	val createTime: Long

	/**
	 * 结束时间
	 */
	val deadline: Long?

	/**
	 * 题单题目数量
	 */
	val problemCount: Int

	/**
	 * 收藏数量
	 */
	val markCount: Int

	/**
	 * 题单 ID
	 */
	val id: Int

	/**
	 * 题单标题
	 */
	val title: String

	/**
	 * 题单类型（官方或其他）
	 */
	val type: Int

	/**
	 * 题单提供者
	 */
	val provider: BaseUser
}

data class BaseTrainingImpl(override val createTime: Long, override val deadline: Long?, override val problemCount: Int, override val markCount: Int, override val id: Int, override val title: String, override val type: Int, override val provider: BaseUser) : BaseTraining {
	companion object Serializer : Deserializable<BaseTraining>(BaseTraining::class), JsonDeserializer<BaseTraining> {
		override fun deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): BaseTraining {
			return context.deserialize(json, BaseTrainingImpl::class.java)
		}
	}
}

@JsonAdapter(UserScoreImpl.Serializer::class)
interface UserScore {
	companion object;

	/**
	 * 总分
	 */
	val totalScore: Int

	/**
	 * 各个题目的得分
	 */
	val score: Map<ProblemID, Int?>
}

data class UserScoreImpl(override val totalScore: Int, override val score: Map<ProblemID, Int?>) : UserScore {
	companion object Serializer : Deserializable<UserScore>(UserScore::class), JsonDeserializer<UserScore> {
		override fun deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): UserScore {
			val delegate = json.asJsonObject.delegate
			val totalScore: Int by delegate
			val score: JsonObject by delegate
			val scoreMap = score.keySet().map {
				val value = score[it]

				it to if (value.isJsonNull) null else value.asInt
			}.toMap()

			return UserScoreImpl(totalScore, scoreMap)
		}
	}
}

@JsonAdapter(TrainingInfoImpl.Serializer::class)
interface TrainingInfo : BaseTraining {
	companion object;

	/**
	 * 题单描述
	 */
	val description: String

	/**
	 * 是否收藏
	 */
	val marked: Boolean

	/**
	 * 题单内题目
	 */
	val problems: List<BaseProblem>

	/**
	 * 用户分数
	 */
	val userScore: UserScore?
}

data class TrainingInfoImpl(override val description: String, override val marked: Boolean, override val problems: List<BaseProblem>, override val userScore: UserScore?, val base: BaseTraining) : TrainingInfo, BaseTraining by base {
	companion object Serializer : Deserializable<TrainingInfo>(TrainingInfo::class), JsonDeserializer<TrainingInfo> {
		override fun deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): TrainingInfo {
			val training = json.asJsonObject.delegate
			val description: String by training
			val marked: Boolean by training
			val problems: JsonArray by training
			val base: BaseTraining = context.deserialize(json, BaseTraining::class.java)
			val userScore: UserScore? = context.deserialize(training.original["userScore"], UserScore::class.java)

			val processedProblems = problems.map {
				BaseProblemImpl(it.asJsonObject["problem"].asJsonObject)
			}

			return TrainingInfoImpl(description, marked, processedProblems, userScore, base)
		}
	}
}