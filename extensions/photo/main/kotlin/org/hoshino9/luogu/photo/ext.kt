@file:JvmName("PhotoUtils")

package org.hoshino9.luogu.photo

import com.google.gson.JsonArray
import com.google.gson.JsonObject
import io.ktor.client.call.receive
import io.ktor.client.request.get
import io.ktor.http.ContentType
import okhttp3.MediaType
import okhttp3.MultipartBody
import okhttp3.Request
import okhttp3.RequestBody
import org.hoshino9.luogu.LuoGu
import org.hoshino9.luogu.LuoGuUtils.baseUrl
import org.hoshino9.luogu.user.LoggedUser
import org.hoshino9.luogu.utils.*
import java.io.File

/**
 * 生成图片上传链接
 * @param watermark 水印类型，0 为无水印，1 为仅 Logo，2 为 Logo + 用户名
 * @param verifyCode 验证码
 * @return 返回一个 Json 对象
 */
suspend fun LuoGu.generateUploadLink(watermark: Int = 1, verifyCode: String): JsonObject {
	return client.get<String>("$baseUrl/api/image/generateUploadLink?watermarkType=$watermark&captcha=$verifyCode").run(::json)
}

/**
 * TODO: replace OkHttpClient with Ktor HttpClient
 * 上传图片
 *
 * @param watermark 水印
 * @param photo 图片的文件对象
 * @param verifyCode 验证码
 * @param contentType 请求的 Content-Type，需要根据图片的格式来进行选择
 * @return 返回图片 id
 *
 * @see [generateUploadLink]
 */
suspend fun LuoGu.pushPhoto(watermark: Int = 1, photo: File, verifyCode: String, contentType: ContentType): String {
	return generateUploadLink(watermark, verifyCode)["uploadLink"].asJsonObject.provider.let { provider ->
		val accessKeyID: String by provider.provide()
		val callback: String by provider.provide()
		val host: String by provider.provide()
		val policy: String by provider.provide()
		val signature: String by provider.provide()

		val body = MultipartBody.Builder()
				.setType(MultipartBody.FORM)
				.addFormDataPart("signature", signature)
				.addFormDataPart("callback", callback)
				.addFormDataPart("success_action_status", "200")
				.addFormDataPart("OSSAccessKeyId", accessKeyID)
				.addFormDataPart("policy", policy)
				.addFormDataPart("key", "upload/image_hosting/__upload/\${filename}")
				.addFormDataPart("name", photo.name)
				.addFormDataPart("file", photo.name, RequestBody.create(MediaType.get(contentType.contentType + "/" + contentType.contentSubtype), photo))
				.build()

//		val body0 = MultiPartFormDataContent(formData {
//			append("\"signature\"", signature)
//			append("\"callback\"", callback)
//			append("\"success_action_status\"", "200")
//			append("\"OSSAccessKeyId\"", accessKeyID)
//			append("\"policy\"", policy)
//			append("\"key\"", "upload/image_hosting/__upload/\${filename}")
//			append("\"name\"", photo.name)
//
//			append("\"file\"", "\"${photo.name}\"", contentType, photo.length()) {
//				writeFully(photo.readBytes())
//			}
//		})

		client.toOkHttpClient().newCall(
				Request.Builder()
						.url(host)
						.post(body)
						.build()
		).execute().let { resp ->
			if (! resp.isSuccessful) throw IllegalStateException()

			json(resp.body() !!.string()) {
				get("image").asJsonObject["id"].asString
			}
		}

//		luogu.client.post<HttpResponse>(host) {
//			referer("image")
//			this.body = body0
//		}.let {
//			String(it.content.toByteArray()).run(::println)

//			""
//			json(it) {
//				get("image").asJsonObject["id"].asString
//			}
//		}
	}
}

/**
 * 图床列表
 * @return 返回一个图片的列表
 *
 * @see IPhoto
 */
fun LuoGu.photoList(page: Int): List<IPhoto> {
	return PhotoListPage(page, client).list
}

/**
 * 删除图片
 * @param photo 需要删除的图片
 */
suspend fun LuoGu.deletePhoto(photo: List<String>) {
	photo.run {
		val json = JsonObject().apply {
			add("images", JsonArray().apply {
				photo.forEach(::add)
			})
		}.asParams

		apiPost("api/image/delete") {
			referer("image")
			body = json
		}.receive<String>()
	}
}

///**
// * 获取图床图片列表
// * @param list 图片列表的元素
// * @return 返回一个 Photo 列表
// *
// * @see Photo
// */
//private fun getPhotos(list: Element): List<Photo> {
//	return list.getElementsByClass("lg-table-row").map {
//		Photo.Factory(it).newInstance()
//	}
//}


/**
 * 上传图片到**你谷**
 * @param file 图片的 File 对象
 * @throws IllegalStatusCodeException 当 api 状态码不为 201 时抛出
 * @throws IllegalStatusCodeException 当 请求状态码不为 200 时抛出
 *
 * @see File
 */
//fun LoggedUser.postPhoto(file: File) {
//	luogu.executePost("app/upload", MultipartBody.Builder()
//			.setType(MultipartBody.FORM)
//			.addFormDataPart("picupload", file.name, file.asRequestBody("application/octet-stream".toMediaTypeOrNull()))
//			.build(),
//			referer("app/upload")) { resp ->
//		resp.assert()
//		val content = resp.strData
//		json(content) {
//			if (this["code"]?.asInt != 201) throw IllegalStatusCodeException(this["code"])
//		}
//	}
//}