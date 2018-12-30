@file:Suppress("unused", "UNUSED_PARAMETER")

package org.hoshino9.luogu

import okhttp3.*
import org.hoshino9.luogu.LuoGuUtils.baseUrl
import org.hoshino9.luogu.data.CodeObject
import org.hoshino9.luogu.benben.BenBenType
import org.hoshino9.luogu.comment.Comment
import org.hoshino9.luogu.data.SliderPhoto
import org.hoshino9.luogu.training.DefaultTrainingPage
import org.hoshino9.luogu.training.TrainingPage
import org.hoshino9.luogu.problem.Problem
import org.hoshino9.luogu.problem.ProblemListPage
import org.hoshino9.luogu.problem.ProblemSearchConfig
import org.hoshino9.luogu.user.LoggedUser
import org.hoshino9.luogu.utils.*
import org.jsoup.Jsoup
import java.io.OutputStream

/**
 * # LuoGU
 * **你谷**客户端类
 */
@Suppress("MemberVisibilityCanBePrivate")
open class LuoGu @JvmOverloads constructor(val client : OkHttpClient = defaultClient) {
	companion object {
		@JvmName("newInstance")
		operator fun invoke(clientId : String, uid : String) : LuoGu = LuoGu().apply {
			this.clientId = clientId
			this.myuid = uid

			refresh()
		}
	}

	var myuid : String
		get() {
			return client.cookieJar().loadForRequest(LuoGuUtils.httpUrl).firstOrNull { it.name() == "_uid" }?.value().orEmpty()
		}
		set(value) {
			client.cookieJar().saveFromResponse(
					LuoGuUtils.httpUrl, listOf(Cookie.parse(LuoGuUtils.httpUrl, "_uid=$value"))
			)
		}

	var clientId : String
		get() {
			return client.cookieJar().loadForRequest(LuoGuUtils.httpUrl).firstOrNull { it.name() == "__client_id" }?.value().orEmpty()
		}
		set(value) {
			client.cookieJar().saveFromResponse(
					LuoGuUtils.httpUrl, listOf(Cookie.parse(LuoGuUtils.httpUrl, "__client_id=$value"))
			)
		}

	/**
	 * 返回 **你谷** 主页源代码
	 */
	val homePage : String
		get() = executeGet { resp ->
			if (resp.isSuccessful) {
				resp.data !!
			} else throw IllegalStatusCodeException(resp.code())
		}

	/**
	 * 一个奇怪的Token, 似乎十分重要, 大部分操作都需要这个
	 */
	val csrfToken : String
		get() {
			return executeGet { resp ->
				resp.assert()
				LuoGuUtils.getCsrfTokenFromPage(Jsoup.parse(resp.data !!))
			}
		}

	/**
	 * 主站滚动图片
	 */
	val sliderPhotos : List<SliderPhoto>
		get() {
			return executeGet { resp ->
				resp.assert()

				resp.data !!.run(Jsoup::parse).run(LuoGuUtils::getSliderPhotosFromPage)
			}
		}

	val trainingPage : TrainingPage
		get() {
			return DefaultTrainingPage(this)
		}

	/**
	 * 获得当前客户端登录的用户
	 */
	lateinit var loggedUser : LoggedUser

	/**
	 * 刷新客户端状态
	 */
	@Synchronized
	fun refresh() {
		loggedUser = LoggedUser(this)
	}

	/**
	 * 获取验证码
	 * @param out 输出流, 将会把验证码**图片**输出到这个流里
	 */
	fun verifyCode(out : OutputStream) {
		executeGet("download/captcha") { resp ->
			resp.assert()
			resp.body() !!.byteStream().copyTo(out)
		}
	}

	/**
	 * 登录**你谷**
	 * @param account 账号
	 * @param password 密码
	 * @param verifyCode 验证码, 通过 [LuoGu.verifyCode] 获得
	 * @throws IllegalAPIStatusCodeException 当登录失败时抛出
	 * @throws IllegalStatusCodeException 当请求码错误时抛出
	 * @return 返回一个 LuoGuLoginResult 对象
	 *
	 * @see LuoGu.verifyCode
	 * @see LoggedUser
	 * @see IllegalAPIStatusCodeException
	 * @see IllegalStatusCodeException
	 */
	fun login(account : String, password : String, verifyCode : String) {
		val params = listOf(
				"username" to account,
				"password" to password,
				"cookie" to "0",
				"redirect" to "",
				"two_factor" to "undefined",
				"verify" to verifyCode
		).params()

		Request.Builder()
				.url("$baseUrl/login/loginpage")
				.post(params)
				.build()
				.run(client::newCall)
				.execute().let { resp ->
					resp.assert()
					val content = resp.data !!

					val data = globalGson.fromJson(content, CodeObject::class.java)

					if (data.code != 200) throw IllegalAPIStatusCodeException(data.code, data.message)
					refresh()
				}
	}

	fun logout() {
		executeGet("login/logout?uid=$myuid") { resp ->
			resp.assert()
		}
	}

	/**
	 * 公开的犇犇列表
	 * @param page 页面序号
	 * @return 返回一个评论列表
	 * @see Comment
	 */
	@JvmOverloads
	fun publicBenben(page : Int = 1) : List<Comment> = LoggedUser(this, "Internal").getBenben(BenBenType.ALL, page)

	/**
	 * 题目列表
	 * @param page 页数, 默认为 **1**
	 * @param filter 过滤器
	 * @throws IllegalStatusCodeException
	 * @return 返回题目列表
	 *
	 * @see Problem
	 * @see ProblemSearchConfig
	 */
	@JvmOverloads
	fun problemList(page : Int = 1, filter : ProblemSearchConfig = ProblemSearchConfig()) : List<Problem> {
		return executeGet("problemnew/lists?$filter&page=$page") { resp ->
			resp.assert()
			ProblemListPage(Jsoup.parse(resp.data !!)).list()
		}
	}
}