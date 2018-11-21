@file:Suppress("unused", "UNUSED_PARAMETER")

package org.hoshino9.luogu

import okhttp3.*
import org.hoshino9.luogu.benben.BenBenType
import org.hoshino9.luogu.benben.LuoGuComment
import org.hoshino9.luogu.practice.PracticeBlock
import org.hoshino9.luogu.problems.Problem
import org.hoshino9.luogu.problems.ProblemListPage
import org.hoshino9.luogu.problems.ProblemSearchConfig
import org.hoshino9.okhttp.LuoGuOnlyCookieJar
import org.json.JSONObject
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
		operator fun invoke(clientId : String) : LuoGu = LuoGu().apply {
			client.cookieJar().saveFromResponse(HttpUrl.get(baseUrl), listOf(
					Cookie.Builder()
							.domain(LuoGuOnlyCookieJar.domain)
							.name("__client_id")
							.value(clientId)
							.build()
			))

			refresh()
		}
	}

	/**
	 * 返回 **你谷** 主页源代码
	 */
	val homePage : String
		get() = getExecute { resp ->
			if (resp.isSuccessful) {
				resp.data !!
			} else throw StatusCodeException(resp.code())
		}

	/**
	 * 一个奇怪的Token, 似乎十分重要, 大部分操作都需要这个
	 */
	val csrfToken : String
		get() {
			return getExecute { resp ->
				resp.assert()
				LuoGuUtils.getCsrfTokenFromPage(Jsoup.parse(resp.data !!))
			}
		}

	val sliderPhotos : List<SliderPhoto>
		get() {
			return getExecute { resp ->
				resp.assert()

				resp.data !!.run(Jsoup::parse).run(LuoGuUtils::getSliderPhotosFromPage)
			}
		}

	val practiceList : List<PracticeBlock>
		get() {
			TODO()
		}

	/**
	 * 获得当前客户端登录的用户
	 */
	lateinit var loggedUser : LuoGuLoggedUser

	/**
	 * 刷新客户端状态
	 */
	fun refresh() {
		loggedUser = LuoGuLoggedUser(this)
	}

	/**
	 * 获取验证码
	 * @param out 输出流, 将会把验证码**图片**输出到这个流里
	 */
	fun verifyCode(out : OutputStream) {
		getExecute("download/captcha") { resp ->
			resp.assert()
			resp.body() !!.byteStream().copyTo(out)
		}
	}

	/**
	 * 登录**你谷**
	 * @param account 账号
	 * @param password 密码
	 * @param verifyCode 验证码, 通过 LuoGu::verifyCode 获得
	 * @throws APIStatusCodeException 当登录失败时抛出
	 * @throws StatusCodeException 当请求码错误时抛出
	 * @return 返回一个 LuoGuLoginResule 对象
	 *
	 * @see LuoGu.verifyCode
	 * @see LuoGuLoggedUser
	 * @see APIStatusCodeException
	 * @see StatusCodeException
	 */
	fun login(account : String, password : String, verifyCode : String) {
		val params = mapOf(
				"username" to account,
				"password" to password,
				"cookie" to "0",
				"redirect" to "",
				"two_factor" to "undefined",
				"verify" to verifyCode
		).params()

		return Request.Builder()
				.url("$baseUrl/login/loginpage")
				.post(params)
				.build()
				.run(client::newCall)
				.execute().let { resp ->
					resp.assert()
					val content = resp.data !!
					json(content) {
						val code : Int = getInt("code")
						val msg : String = getString("message")

						if (code != 200) throw APIStatusCodeException(code, msg)
						refresh()
					}
				}
	}

	/**
	 * 公开的犇犇列表
	 * @param page 页面序号
	 * @return 返回一个评论列表
	 * @see LuoGuComment
	 */
	@JvmOverloads
	fun publicBenben(page : Int = 1) : List<LuoGuComment> = LuoGuLoggedUser(this, "Internal").benben(BenBenType.ALL, page)

	/**
	 * 题目列表
	 * @param page 页数, 默认为 **1**
	 * @param filter 过滤器
	 * @throws StatusCodeException
	 * @return 返回题目列表
	 *
	 * @see Problem
	 * @see ProblemSearchConfig
	 */
	@JvmOverloads
	fun problemList(page : Int = 1, filter : ProblemSearchConfig = ProblemSearchConfig()) : List<Problem> {
		return getExecute("problemnew/lists?$filter&page=$page") { resp ->
			resp.assert()
			ProblemListPage(Jsoup.parse(resp.data !!)).list()
		}
	}
}