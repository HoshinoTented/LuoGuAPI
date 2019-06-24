@file:JvmName("PhotoUtils")

package org.hoshino9.luogu.photo

import org.hoshino9.luogu.user.LoggedUser
import org.hoshino9.luogu.utils.*
import org.jsoup.Jsoup
import org.jsoup.nodes.Element

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
fun getPhotos(list: Element): List<Photo> {
	return list.getElementsByClass("lg-table-row").map {
		Photo.Factory(it).newInstance()
	}
}