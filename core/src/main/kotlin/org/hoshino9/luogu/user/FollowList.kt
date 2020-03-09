package org.hoshino9.luogu.user

import com.google.gson.*
import com.google.gson.annotations.JsonAdapter
import io.ktor.client.request.get
import kotlinx.coroutines.runBlocking
import org.hoshino9.luogu.baseUrl
import org.hoshino9.luogu.utils.*
import java.lang.reflect.Type

@JsonAdapter(FollowListUserImpl.Serializer::class)
interface FollowListUser : BaseUser {
	companion object;
	val blogAddress: String?
	val followingCount: Int
	val followerCount: Int
	val ranking: Int?
	val userRelationship: Int
	val reverseUserRelationship: Int
	val passedProblemCount: Int
	val submittedProblemCount: Int
}

data class FollowListUserImpl(override val blogAddress: String?, override val followingCount: Int, override val followerCount: Int, override val ranking: Int?, override val userRelationship: Int, override val reverseUserRelationship: Int, override val passedProblemCount: Int, override val submittedProblemCount: Int, val baseUser: BaseUser) : BaseUser by baseUser, FollowListUser {
	companion object Serializer : Deserializable<FollowListUser>(FollowListUser::class), JsonDeserializer<FollowListUser> {
		override fun deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): FollowListUser {
			val source = json.asJsonObject
			val delegate = source.delegate

			val blogAddress: String? by delegate
			val followingCount: Int by delegate
			val followerCount: Int by delegate
			val ranking: Int? by delegate
			val userRelationship: Int by delegate
			val reverseUserRelationship: Int by delegate
			val passedProblemCount: Int by delegate
			val submittedProblemCount: Int by delegate
			val baseUser: BaseUser = context.deserialize(json, BaseUser::class.java)

			return FollowListUserImpl(blogAddress, followingCount, followerCount, ranking, userRelationship, reverseUserRelationship, passedProblemCount, submittedProblemCount, baseUser)
		}
	}
}

@JsonAdapter(FollowListImpl.Serializer::class)
interface FollowList {
	companion object;

	enum class Type {
		Followings,
		Followers
	}

	val result: List<FollowListUser>
	val count: Int
}

data class FollowListImpl(override val result: List<FollowListUser>, override val count: Int) : FollowList {
	companion object Serializer : Deserializable<FollowList>(FollowList::class), JsonDeserializer<FollowList> {
		override fun deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext?): FollowList {
			val users = json.asJsonObject
			val usersDlgt = users.delegate
			val count: Int by usersDlgt
			val result: JsonArray by usersDlgt
			val list = result.map {
				FollowListUserImpl(it)
			}

			return FollowListImpl(list, count)
		}
	}
}

suspend operator fun FollowList.Companion.invoke(uid: Int, page: Int, type: FollowList.Type, client: HttpClient = emptyClient): FollowList {
	val url = "$baseUrl/fe/api/user/${type.name.toLowerCase()}?user=$uid&page=$page"
	val data = json(client.get(url)).getAsJsonObject("users")

	return FollowListImpl(data)
}