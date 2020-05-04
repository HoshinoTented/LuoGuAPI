package org.hoshino9.luogu.problem

import com.google.gson.JsonDeserializationContext
import com.google.gson.JsonElement
import com.google.gson.annotations.JsonAdapter
import org.hoshino9.luogu.LuoGuClient
import org.hoshino9.luogu.page.AbstractLuoGuClientPage
import org.hoshino9.luogu.page.ListPage
import org.hoshino9.luogu.page.PageBuilder
import org.hoshino9.luogu.user.BaseUser
import org.hoshino9.luogu.utils.JsonDeserializable


@JsonAdapter(SolutionImpl.Serializer::class)
interface Solution {
	val author: BaseUser
	val commentCount: Int
	val content: String
	val contentDescription: String
	val currentUserVoteType: Int
	val id: Int
	val identifier: String
	val postTime: Int
	val status: Int
	val thumbUp: Int
	val title: String
	val type: String
}

data class SolutionImpl(override val author: BaseUser, override val commentCount: Int, override val content: String, override val contentDescription: String, override val currentUserVoteType: Int, override val id: Int, override val identifier: String, override val postTime: Int, override val status: Int, override val thumbUp: Int, override val title: String, override val type: String) : Solution {
	companion object Serializer : JsonDeserializable<Solution>(Solution::class) {
		override fun deserialize(json: JsonElement, typeOfT: java.lang.reflect.Type, context: JsonDeserializationContext): Solution {
			return context.deserialize(json, SolutionImpl::class.java)
		}
	}
}

@JsonAdapter(SolutionListPageImpl.Serializer::class)
interface SolutionListPage : ListPage {
	val result: List<Solution>
}

data class SolutionListPageImpl(override val result: List<Solution>, override val count: Int, override val perPage: Int) : SolutionListPage {
	companion object Serializer : JsonDeserializable<SolutionListPage>(SolutionListPage::class) {
		override fun deserialize(json: JsonElement, typeOfT: java.lang.reflect.Type, context: JsonDeserializationContext): SolutionListPage {
			return context.deserialize(json.asJsonObject["solutions"], SolutionListPageImpl::class.java)
		}
	}
}

// TODO: 题解需要登录才能获取，这里加个判断之类的
class SolutionPageBuilder(val pid: String, client: LuoGuClient) : AbstractLuoGuClientPage(client), PageBuilder<SolutionListPage> {
	override val url: String = "https://www.luogu.com.cn/problem/solution/$pid"

	override fun build(): SolutionListPage {
		return SolutionListPageImpl(currentData)
	}
}