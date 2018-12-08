package org.hoshino9.luogu.photo

import org.jsoup.nodes.Element

object PhotoUtils {
	/**
	 * 获取图床图片列表
	 * @param list 图片列表的元素
	 * @return 返回一个 Photo 列表
	 *
	 * @see Photo
	 */
	fun getPhotos(list : Element) : List<Photo> {
		return list.getElementsByClass("lg-table-row").map {
			Photo(it)
		}
	}
}