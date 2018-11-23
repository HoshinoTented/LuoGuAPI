package org.hoshino9.luogu.photo

import org.jsoup.nodes.Element

object LuoGuPhotoUtils {
	/**
	 * 获取图床图片列表
	 * @param list 图片列表的元素
	 * @return 返回一个 LuoGuPhoto 列表
	 *
	 * @see LuoGuPhoto
	 */
	fun getPhotos(list : Element) : List<LuoGuPhoto> {
		return list.getElementsByClass("lg-table-row").map {
			LuoGuPhoto.Builder().element(it).build()
		}
	}
}