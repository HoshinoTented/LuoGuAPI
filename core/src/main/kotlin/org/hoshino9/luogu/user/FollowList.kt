package org.hoshino9.luogu.user

import com.google.gson.*
import com.google.gson.annotations.JsonAdapter
import io.ktor.client.request.get
import kotlinx.coroutines.runBlocking
import org.hoshino9.luogu.LuoGuUtils.baseUrl
import org.hoshino9.luogu.utils.*
import java.lang.reflect.Type

@JsonAdapter(FollowListUser.Serializer::class)
interface IFollowListUser : IBaseUser {
	val blogAddress: String?
	val followingCount: Int
	val followerCount: Int
	val ranking: Int?
	val userRelationship: Int
	val reverseUserRelationship: Int
	val passedProblemCount: Int
	val submittedProblemCount: Int
}

data class FollowListUser(override val blogAddress: String?, override val followingCount: Int, override val followerCount: Int, override val ranking: Int?, override val userRelationship: Int, override val reverseUserRelationship: Int, override val passedProblemCount: Int, override val submittedProblemCount: Int, val baseUser: IBaseUser) : IBaseUser by baseUser, IFollowListUser {
	companion object Serializer : Deserializable<IFollowListUser>(IFollowListUser::class), JsonDeserializer<IFollowListUser> {
		override fun deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): IFollowListUser {
			val source = json.asJsonObject

			val blogAddress: String? = source["blogAddress"].ifNull()?.asString
			val followingCount: Int = source["followingCount"].asInt
			val followerCount: Int = source["followerCount"].asInt
			val ranking: Int? = source["ranking"].ifNull()?.asInt
			val userRelationship: Int = source["userRelationship"].asInt
			val reverseUserRelationship: Int = source["reverseUserRelationship"].asInt
			val passedProblemCount: Int = source["passedProblemCount"].asInt
			val submittedProblemCount: Int = source["submittedProblemCount"].asInt
			val baseUser: IBaseUser = context.deserialize(json, IBaseUser::class.java)

			return FollowListUser(blogAddress, followingCount, followerCount, ranking, userRelationship, reverseUserRelationship, passedProblemCount, submittedProblemCount, baseUser)
		}
	}
}

class FollowList(val user: IUser, val page: Int, val type: Type, val client: HttpClient = emptyClient) {
	enum class Type {
		Followings,
		Followers
	}

	private val url = "$baseUrl/fe/api/user/${type.name.toLowerCase()}?user=${user.uid}&page=$page"
	private val data: JsonObject = runBlocking { json(client.get(url)) }
	private val users = data["users"].asJsonObject
	private val result: JsonArray = users["result"].asJsonArray

	val count: Int get() = users["count"].asInt
	val list: List<IFollowListUser>
		get() {
			return result.map {
				FollowListUser(it)
			}
		}
}