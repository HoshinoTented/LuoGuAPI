@file:JvmName("PhotoUtils")

package org.hoshino9.luogu.photo

import com.google.gson.JsonArray
import com.google.gson.JsonObject
import okhttp3.MediaType
import okhttp3.MediaType.Companion.toMediaTypeOrNull
import okhttp3.MultipartBody
import okhttp3.RequestBody.Companion.asRequestBody
import org.hoshino9.luogu.IllegalStatusCodeException
import org.hoshino9.luogu.user.LoggedUser
import org.hoshino9.luogu.utils.*
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import java.io.File

/**
 * 生成图片上传链接
 * @param watermark 水印类型，0 为无水印，1 为仅 Logo，2 为 Logo + 用户名
 * @param verifyCode 验证码
 * @return 返回一个 Json 对象
 */
fun LoggedUser.generateUploadLink(watermark: Int = 1, verifyCode: String): JsonObject {
	return luogu.executeGet("api/image/generateUploadLink?watermarkType=1&captcha=$verifyCode") {
		it.assert()

		json(it.strData)
	}
}

/**
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
fun LoggedUser.pushPhoto(watermark: Int = 1, photo: File, verifyCode: String, contentType: MediaType): String {
	return generateUploadLink(watermark, verifyCode)["uploadLink"].asJsonObject.delegate.let { dlgt ->
		val accessKeyID: String by dlgt
		val callback: String by dlgt
		val host: String by dlgt
		val policy: String by dlgt
		val signature: String by dlgt

		val body = MultipartBody.Builder()
				.setType(MultipartBody.FORM)
				.addFormDataPart("signature", signature)
				.addFormDataPart("callback", callback)
				.addFormDataPart("success_action_status", "200")
				.addFormDataPart("OSSAccessKeyId", accessKeyID)
				.addFormDataPart("policy", policy)
				.addFormDataPart("key", "upload/image_hosting/\${filename}")
				.addFormDataPart("name", photo.name)
				.addFormDataPart("file", photo.name, photo.asRequestBody(contentType))
				.build()

		luogu.client.executePost(host, body, referer("https://www.luogu.org/image")) {
			it.assert()

			json(it.strData) {
				get("id").asString
			}
		}
	}
}

/**
 * 图床列表
 * @return 返回一个图片的列表
 *
 * @see Photo
 */
fun LoggedUser.photoList(page: Int): List<Photo.Factory> {
	return PhotoListPage(page, luogu.client).list
}

/**
 * 删除图片
 * @param photo 需要删除的图片
 */
fun LoggedUser.deletePhoto(photo: List<String>) {
	photo.run {
		val params = JsonObject().apply {
			add("images", JsonArray().apply {
				photo.forEach(::add)
			})
		}.params()

		luogu.executePost("api/image/delete", body = params, headers = referer("image")) {
			it.assert()
		}
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