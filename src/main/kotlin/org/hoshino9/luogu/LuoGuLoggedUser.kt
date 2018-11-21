package org.hoshino9.luogu

import okhttp3.MediaType
import okhttp3.MultipartBody
import okhttp3.RequestBody
import org.hoshino9.luogu.benben.LuoGuComment
import org.hoshino9.luogu.benben.BenBenType
import org.hoshino9.luogu.paste.BasicPaste
import org.hoshino9.luogu.paste.Paste
import org.hoshino9.luogu.photo.LuoGuPhoto
import org.hoshino9.luogu.problems.Solution
import org.hoshino9.luogu.results.LuoGuSignedInStatus
import org.json.JSONObject
import org.jsoup.Jsoup
import java.io.File

/**
 * **你谷**用户类
 * 但仅限于已登录的用户
 * 未登录的用户请用 `String` 代替
 * 等到 Kotlin1.3 可以改用 `inline class`
 */
@Suppress("MemberVisibilityCanBePrivate", "unused", "UNUSED_PARAMETER")
open class LuoGuLoggedUser(val luogu : LuoGu, uid : String) : LuoGuUser(uid) {
	companion object {
		/**
		 * 实例化一个 LuoGuLoggedUser 对象
		 * @param luogu 已经登陆过的洛谷客户端
		 * @return 返回一个 LuoGuLoggedUser 对象
		 *
		 * @see LuoGuUtils.getUserIdFromPage
		 */
		@Throws(StatusCodeException::class)
		@JvmName("newInstance")
		operator fun invoke(luogu : LuoGu) : LuoGuLoggedUser {
			luogu.getExecute { resp ->
				resp.assert()
				val content = resp.data !!

				return LuoGuLoggedUser(luogu, Jsoup.parse(content).run(LuoGuUtils::getUserIdFromPage) ?: throw StatusException("no logged in"))
			}
		}
	}

	/**
	 * 获取签到状态信息
	 * @return 返回一个签到状态类
	 * @throws StatusException 未签到时抛出
	 *
	 * @see LuoGuSignedInStatus
	 */
	@get:Throws(StatusException::class)
	val signInStatus : LuoGuSignedInStatus
		get() {
			val doc = luogu.homePage.run(Jsoup::parse)
			val node = doc.body().children()
					.getOrNull(1)?.children()
					?.getOrNull(1)?.children()
					?.getOrNull(1)?.children()
					?.first()?.children()
					?.first()?.children()
					?.first()?.children()
					?.first()?.children()
					?.getOrNull(1) ?: throw NoSuchElementException()
			return LuoGuSignedInStatus(node.children())
		}

	/**
	 * **你谷**签到
	 */
	@Throws(StatusCodeException::class)
	fun signIn() {
		return luogu.getExecute("index/ajax_punch") { resp ->
			resp.assert()
		}
	}

	/**
	 * 获取犇犇列表
	 * @param type 犇犇的类型
	 * @param page 页数, 默认为`1`
	 * @return 返回一个犇犇的列表
	 *
	 * @see LuoGuComment
	 * @see BenBenType
	 */
	@JvmOverloads
	@Throws(StatusCodeException::class)
	fun benben(type : BenBenType, page : Int = 1) : List<LuoGuComment> {
		luogu.getExecute("feed/${type.toString().toLowerCase()}?page=$page") { resp ->
			resp.assert()
			val content = resp.data !!
			return LuoGuUtils.getBenben(Jsoup.parse(content).body())
		}
	}

	/**
	 * 剪切板
	 * @param code `markdown` 代码
	 * @param public 是否公开, 默认 **true**
	 * @return 返回剪切板的代码
	 */
	@JvmOverloads
	@Throws(StatusCodeException::class, APIStatusCodeException::class)
	fun paste(code : String, public : Boolean = true) : Paste {
		luogu.postExecute("paste/post", mapOf(
				"content" to code,
				"verify" to "",
				"public" to if (public) "1" else "0"
		).params()) { resp ->
			resp.assert()

			val content = resp.data !!
			JSONObject(content).let {
				val mStatusCode = it.optInt("status")
				val mData = it.optString("data")
				if (it.optInt("status") == 200) {
					return mData.run(::BasicPaste)
				} else {
					throw APIStatusCodeException(mStatusCode, mData)
				}
			}
		}
	}

	fun pasteList() : List<Paste> {
		val regex = Regex("""https://www.luogu.org/paste/(\w+)""")
		luogu.getExecute("paste") { resp ->
			resp.assert()
			val content = resp.data !!

			return Jsoup.parse(content).toString().run { regex.findAll(this) }.map {
				BasicPaste(it.groupValues[1])
			}.toList()
		}
	}

	/**
	 * 发射犇犇
	 * @param text 犇犇内容
	 */
	@Throws(StatusCodeException::class, APIStatusCodeException::class)
	fun postBenben(text : String) {
		luogu.postExecute("api/feed/postBenben", mapOf("content" to text).params()) { resp ->
			resp.assert()
			val content = resp.data !!

			JSONObject(content).run {
				val status = getInt("status")
				val data = get("data")
				if (status != 200) throw APIStatusCodeException(status, data.toString())
			}
		}
	}

	/**
	 * 提交题解
	 * @param solution 题解对象
	 * @return 返回评测结果id
	 *
	 * @see Solution
	 */
	@Throws(StatusCodeException::class, APIStatusCodeException::class)
	fun postSolution(solution : Solution) : String {
		luogu.postExecute("api/problem/submit/${solution.pid}",
				mapOf(
						"code" to solution.code,
						"lang" to solution.language.value.toString(),
						"enableO2" to if (solution.enableO2) "1" else "0",
						"verify" to ""
				).params()
		) { resp ->
			resp.assert()
			val content = resp.data !!

			JSONObject(content).run {
				val status = optInt("status")
				val data = get("data")

				if (status == 200) {
					data as JSONObject
					return data.get("rid").toString()
				} else throw APIStatusCodeException(status, data.toString())
			}
		}
	}

	/**
	 * 上传图片到**你谷**
	 * @param file 图片的 File 对象
	 * @throws APIStatusCodeException 当 api 状态码不为 201 时抛出
	 * @throws StatusCodeException 当 请求状态码不为 200 时抛出
	 *
	 * @see File
	 */
	@Throws(StatusCodeException::class, APIStatusCodeException::class)
	fun postPhoto(file : File) {
		luogu.postExecute("app/upload", MultipartBody.Builder()
				.setType(MultipartBody.FORM)
				.addFormDataPart("picupload", file.name, RequestBody.create(MediaType.parse("application/octet-stream"), file))
				.build()) { resp ->
			resp.assert()
			val content = resp.data !!

			JSONObject(content).run {
				val code = optInt("code")
				if (code != 201) throw APIStatusCodeException(code)
			}
		}
	}

	fun photoList() : List<LuoGuPhoto> {
		luogu.getExecute("app/upload") { resp ->
			resp.assert()

			val page = resp.data !!
			return LuoGuUtils.getPhotos(Jsoup.parse(page))
		}
	}
}