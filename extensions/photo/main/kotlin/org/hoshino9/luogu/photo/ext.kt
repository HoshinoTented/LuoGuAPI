@file:JvmName("PhotoUtils")

package org.hoshino9.luogu.photo

import okhttp3.MediaType
import okhttp3.MultipartBody
import okhttp3.RequestBody
import org.hoshino9.luogu.IllegalAPIStatusCodeException
import org.hoshino9.luogu.IllegalStatusCodeException
import org.hoshino9.luogu.user.LoggedUser
import org.hoshino9.luogu.utils.*
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import java.io.File

/**
 * 图床列表
 * @return 返回一个图片的列表
 *
 * @see Photo
 */
fun LoggedUser.photoList(): List<Photo> {
	luogu.executeGet("app/upload") { resp ->
		resp.assert()

		val page = resp.strData
		return getPhotos(Jsoup.parse(page))
	}
}

/**
 * 删除图片
 * @param photo 需要删除的图片
 */
fun LoggedUser.deletePhoto(photo: Photo) {
	photo.run {
		luogu.executePost("app/upload?delete=1&uploadid=$id", headers = referer("app/upload")) {
			it.assert()
		}
	}
}

/**
 * 获取图床图片列表
 * @param list 图片列表的元素
 * @return 返回一个 Photo 列表
 *
 * @see Photo
 */
private fun getPhotos(list: Element): List<Photo> {
	return list.getElementsByClass("lg-table-row").map {
		Photo.Factory(it).newInstance()
	}
}


/**
 * 上传图片到**你谷**
 * @param file 图片的 File 对象
 * @throws IllegalAPIStatusCodeException 当 api 状态码不为 201 时抛出
 * @throws IllegalStatusCodeException 当 请求状态码不为 200 时抛出
 *
 * @see File
 */
fun LoggedUser.postPhoto(file: File) {
	luogu.executePost("app/upload", MultipartBody.Builder()
			.setType(MultipartBody.FORM)
			.addFormDataPart("picupload", file.name, RequestBody.create(MediaType.parse("application/octet-stream"), file))
			.build(),
			referer("app/upload")) { resp ->
		resp.assert()
		val content = resp.strData
		json(content) {
			if (this["code"]?.asInt != 201) throw IllegalAPIStatusCodeException(this["code"])
		}
	}
}