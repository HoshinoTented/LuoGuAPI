package org.hoshino9.luogu.training

import com.google.gson.JsonDeserializationContext
import com.google.gson.JsonDeserializer
import com.google.gson.JsonElement
import com.google.gson.annotations.JsonAdapter
import org.hoshino9.luogu.user.BaseUser
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